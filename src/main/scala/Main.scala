import java.time.LocalDateTime

import cats.implicits._
import cats.effect.{ExitCode, IO, IOApp, Resource}
import com.redis.RedisClient
import io.circe.Json
import io.circe.optics.JsonPath._
import org.http4s.client._
import org.http4s.client.blaze._
import Ops._
import com.github.kilianB.hashAlgorithms.{AverageHash, AverageKernelHash}
import models.{RedditPostData, Subreddit, TelegramCreds}
import models.RedditPostDataBuilderObj._

import scala.concurrent.ExecutionContext.global
import scala.concurrent.duration._

object Main extends IOApp {
  private val postsOptic = root.data.children.each.data.json

  private val parsePostsData = (l: List[Json]) =>
    l.map(_.as[RedditPostDataBuilder]).mapFilter(_.toOption.map(_.toData)).foldK

  private def getNewPosts(
    clientResourse: Resource[IO, Client[IO]]
  )(requestMaker: Client[IO] => IO[Json]) =
    clientResourse.use(requestMaker).map(postsOptic.getAll).map(parsePostsData)

  private def findNewPosts(
    posts: List[RedditPostData]
  )(implicit r: RedisClient, hasher: AverageHash): IO[List[RedditPostData]] = IO {
    val postsWithHash = posts.map(_.attachImageHash).filter(_.isUnwanted)
    val idsPairs = postsWithHash.map(p => (p.imageHash, p.id)).toMap

    val newIdsStore = postsWithHash.map(_.id) match {
      case head :: tail if tail.nonEmpty =>
        r.sadd("reddit:new_posts", head, tail: _*)
      case head :: _ => r.sadd("reddit:new_posts", head)
      case _         => None
    }

    postsWithHash.mapFilter(_.imageHash) match {
      case head :: tail if tail.nonEmpty =>
        r.sadd("reddit:new_images", head, tail: _*)
      case head :: _ => r.sadd("reddit:new_images", head)
      case _         => None
    }

    val newIds = newIdsStore.flatMap { _ =>
      val postsDiff = r.sdiff("reddit:new_posts", "reddit:posts")
      val images =
        r.sdiff("reddit:new_images", "reddit:images").map(_.map(idsPairs.get))

      r.del("reddit:new_posts", "reddit:new_images") >>
        (postsDiff |+| images)
    }.getOrElse(Set.empty[Option[String]])

    postsWithHash.filter(p => newIds.contains(Some(p.id)))
  }

  private val tryConvertInt = (s: String) =>
    IO(s.toIntOption.toRight[String]("Could not convert a key to Int"))

  private def getSubredditsM(r: RedisClient): IO[Vector[Subreddit]] = IO {
    r.hgetall1[String, String]("reddit:subreddits")
      .map(_.toVector.map { case (n, f) => models.Subreddit(n, f) })
      .getOrElse(Vector.empty[Subreddit])
  }

  implicit class IOOps[A](f: IO[A]) {
    def tee[B](fb: IO[B]): IO[A] = f >>= (v => fb >> IO(v))
    def thenWait(d: FiniteDuration): IO[A] = f.tee(IO.sleep(d))
  }

  override def run(args: List[String]): IO[ExitCode] = {
    implicit val imageHasher: AverageKernelHash = new AverageKernelHash(25) // for now

    val redis = for {
      redisHost <- tryFindKey("REDIS_HOST", Some("localhost"))
      redisPort <- tryFindKey("REDIS_PORT", Some("6379")).flatMapF(tryConvertInt)
      redisPass <- tryFindKey("REDIS_PASS", Some("")).map(p => if (p.isEmpty) None else Some(p))
      redisDB   <- tryFindKey("REDIS_DB", Some("0")).flatMapF(tryConvertInt)
      redis <- IO(
                new RedisClient(
                  redisHost,
                  redisPort,
                  database = redisDB,
                  secret = redisPass
                )
              ).attemptT.leftMap(_.getMessage)
    } yield redis

    val telegramCreds = for {
      botToken <- tryFindKey("TG_BOT_TOKEN")
      chatID   <- tryFindKey("TG_CHAT_ID")
    } yield TelegramCreds(botToken, chatID)

    val client = BlazeClientBuilder[IO](global).resource
    val valueGetter = getNewPosts(client) _

    val makeRun = (r: RedisClient, creds: TelegramCreds) => {
      val posts = getSubredditsM(r)
        .flatMap(_.map(s => valueGetter(s.makeRequestF)).sequence)
        .map(_.foldK.filter(_.postType != models.OTHER))
        .flatMap(findNewPosts(_)(r, imageHasher))

      val sendPosts = posts >>= {
        case posts if posts.nonEmpty =>
          posts.map {
            _.sendMessage(creds.botToken, creds.chatId)(client, r).thenWait(2.seconds)
          }.sequence
        case _ => IO(List("No new posts!"))
      }

      sendPosts.attemptT.leftMap(_.toString)
    }

    val program = for {
      r     <- redis
      creds <- telegramCreds
      run   <- makeRun(r, creds)
    } yield run

    program.value >>= {
      case Right(v) =>
        for {
          messages <- IO(v.map(message => s"${LocalDateTime.now()}: $message"))
          _        <- IO(messages.foreach(println))
        } yield ExitCode.Success
      case Left(exc) => IO.delay(println(exc)) >> IO.pure(ExitCode.Success)
    }
  }
}
