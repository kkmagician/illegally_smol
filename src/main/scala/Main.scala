import java.time.LocalDateTime

import cats.implicits._
import cats.effect.{ExitCode, IO, IOApp, Resource}
import com.redis.RedisClient
import io.circe.Json
import io.circe.optics.JsonPath._
import models.RedditPostDataBuilderObj._
import org.http4s.client._
import org.http4s.client.blaze._
import org.http4s.circe._
import Ops._
import models.RedditPostData

import scala.concurrent.ExecutionContext.global
import scala.concurrent.duration._

object Main extends IOApp {
  private val postsOptic = root.data.children.each.data.json

  def makeRequestF(subbreddit: String)(client: Client[IO]): IO[Json] =
    client.get(s"https://www.reddit.com/r/$subbreddit/new.json")(_.as[Json])

  private val parsePostsData = (l: List[Json]) =>
    l.map(_.as[RedditPostDataBuilder]).mapFilter(_.toOption.map(_.toData)).foldK

  private val createRequestMakers = (subreddits: List[String]) =>
    subreddits.map(arg => makeRequestF(arg) _)

  private def getNewPosts(
    clientResourse: Resource[IO, Client[IO]]
  )(requestMaker: Client[IO] => IO[Json]) =
    clientResourse.use(requestMaker).map(postsOptic.getAll).map(parsePostsData)

  private def findNewPosts(
    posts: List[RedditPostData]
  )(implicit r: RedisClient): List[RedditPostData] = {
    val postIds = posts.map(_.id)
    val newIdsStore = postIds match {
      case head :: tail if tail.nonEmpty =>
        r.sadd("reddit:new_posts", head, tail: _*)
      case head :: _ => r.sadd("reddit:new_posts", head)
      case _         => None
    }

    val newIds = newIdsStore.flatMap { _ =>
      r.sdiff("reddit:new_posts", "reddit:posts")
    }.getOrElse(Set.empty[Option[String]])

    posts.filter(p => newIds.contains(Some(p.id)))
  }

  private val tryConvertInt = (s: String) =>
    IO(s.toIntOption.toRight[String]("Could not convert a key to Int"))

  private val getSubreddits = (r: RedisClient) =>
    IO(
      r.smembers("reddit:subreddits")
        .map(_.toList.flattenOption)
        .getOrElse(List.empty[String])
  )

  implicit class IOOps[A](f: IO[A]) {
    def tee[B](fb: IO[B]): IO[A] = f >>= (v => fb >> IO(v))
    def thenWait(d: FiniteDuration): IO[A] = f.tee(IO.sleep(d))
  }

  case class TelegramCreds(botToken: String, chatId: String)

  override def run(args: List[String]): IO[ExitCode] = {

    val redis = for {
      redisHost <- tryFindKey("REDIS_HOST", Some("localhost"))
      redisPort <- tryFindKey("REDIS_PORT", Some("6379"))
                    .flatMapF(tryConvertInt)
      redisPass <- tryFindKey("REDIS_PASS", Some(""))
                    .map(p => if (p.isEmpty) None else Some(p))
      redisDB <- tryFindKey("REDIS_DB", Some("0")).flatMapF(tryConvertInt)
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
      val posts = getSubreddits(r)
        .map(createRequestMakers)
        .flatMap(_.map(valueGetter).sequence)
        .map(_.foldK.filter(_.postType != models.OTHER))
        .map(findNewPosts(_)(r))

      val sendPosts = posts >>= {
        case posts if posts.nonEmpty =>
          posts.map {
            _.sendMessage(creds.botToken, creds.chatId)(client, r)
              .thenWait(3.seconds)
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
