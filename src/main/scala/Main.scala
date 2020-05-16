import cats.data.EitherT
import cats.implicits._
import cats.effect.{ExitCode, IO, IOApp, Resource}
import com.redis.RedisClient
import io.circe.Json
import io.circe.optics.JsonPath._
import org.http4s.client._
import org.http4s.client.blaze._
import org.http4s.client.dsl.io._
import models.Ops._
import com.github.kilianB.hashAlgorithms.{AverageHash, AverageKernelHash}
import models.{AnalyticsEvent, RedditPostData, Subreddit}
import models.RedditPostDataBuilderObj._
import models.Telegram.TelegramCreds
import org.http4s.{BasicCredentials, Uri}
import org.http4s.Method._
import org.http4s.headers.Authorization

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
    val postsWithHash = posts.map(_.attachImageHash).filter(_.isUnwanted).distinctBy {
      case p: RedditPostData if p.postType == models.IMAGE => p.imageHash
      case p @ _                                           => p.id
    }

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
      for {
        postsDiff <- r.sdiff("reddit:new_posts", "reddit:posts").map(_.toVector.mapFilter(a => a))
        imagesInter <- r.sinter("reddit:new_images", "reddit:images")
                        .map(_.toVector.mapFilter(a => idsPairs.get(a)))
        _ <- r.del("reddit:new_posts", "reddit:new_images")
      } yield postsDiff.diff(imagesInter)
    }.getOrElse(Vector.empty[String])

    postsWithHash.filter(p => newIds.contains(p.id))
  }

  private val tryConvertInt = (s: String) =>
    IO(s.toIntOption.toRight[String]("Could not convert a key to Int"))

  private def getSubredditsM(r: RedisClient): IO[Vector[Subreddit]] = IO {
    r.hgetall1[String, String]("reddit:subreddits")
      .map(_.toVector.map { case (n, f) => models.Subreddit(n, f) })
      .getOrElse(Vector.empty[Subreddit])
  }

  case class ClickHouse(host: String, user: String, pass: String)

  private def prepareChInsert(
    ch: ClickHouse,
    values: Seq[AnalyticsEvent]
  ) =
    Uri
      .fromString(ch.host)
      .map { u =>
        POST(
          "INSERT INTO reddit_tg FORMAT JSONEachRow\n" ++ values.map(_.toJsonLine).mkString("\n"),
          u,
          Authorization(BasicCredentials(ch.user, ch.pass))
        )
      }
      .toOption

  private def sendAnalytics(
    ch: ClickHouse,
    values: Seq[AnalyticsEvent],
    clientRes: Resource[IO, Client[IO]]
  ): EitherT[IO, String, String] =
    prepareChInsert(ch, values) match {
      case Some(req) =>
        clientRes.use { c =>
          c.expect[String](req)
        }.attemptT.leftMap(_.getMessage)
      case None => EitherT.leftT[IO, String]("Wrong CH Host")
    }

  override def run(args: List[String]): IO[ExitCode] = {
    implicit val imageHasher: AverageKernelHash = new AverageKernelHash(26) // for now

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

      val sendPosts = posts >>= { posts =>
        client.use { c =>
          posts.map {
            _.sendMessage(creds.botToken, creds.chatId)(c, r).thenWait(3.seconds)
          }.sequence
        }
      }

      sendPosts.attemptT.leftMap(_.getMessage)
    }

    val clickhouse = for {
      chHost <- tryFindKey("CH_HOST", Some("localhost"))
      chUser <- tryFindKey("CH_USER", Some("default"))
      chPass <- tryFindKey("CH_PASS", Some(""))
    } yield ClickHouse(chHost, chUser, chPass)

    val program = for {
      r     <- redis
      ch    <- clickhouse
      creds <- telegramCreds
      run   <- makeRun(r, creds)
      ch    <- sendAnalytics(ch, run, client)
      _ = println(ch)
    } yield run

    program.value >>= {
      case Right(v) =>
        for {
          jsons <- IO.pure(v.map(_.toJsonLine).mkString("\n"))
          _     <- IO.delay(println(jsons))
        } yield ExitCode.Success
      case Left(exc) => IO.delay(println(exc)) >> IO.pure(ExitCode.Success)
    }
  }
}
