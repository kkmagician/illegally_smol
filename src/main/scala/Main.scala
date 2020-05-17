import java.time.LocalDateTime

import cats.data.EitherT
import cats.implicits._
import cats.effect.{ExitCode, IO, IOApp, Resource}

import com.redis.RedisClient
import com.github.kilianB.hashAlgorithms.{AverageHash, AverageKernelHash}

import io.circe.Json
import io.circe.optics.JsonPath._

import org.http4s.client._
import org.http4s.client.blaze._
import org.http4s.client.dsl.io._
import org.http4s.{BasicCredentials, Uri}
import org.http4s.Method._
import org.http4s.headers.Authorization

import models.Ops._
import models.Analytics.{AnalyticsEvent, Clickhouse}
import models.Reddit._
import models.Reddit.RedditPostDataBuilderObj._
import models.Telegram.TelegramCreds

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

  private def addSetRedis(add: Vector[Any], key: String, r: RedisClient): Option[Long] = add match {
    case head +: tail if tail.nonEmpty =>
      r.sadd(key, head, tail: _*)
    case head +: _ => r.sadd(key, head)
    case _         => Some(0)
  }

  private val filterDistinctHash = (posts: Seq[RedditPostData]) =>
    posts.distinctBy {
      case p: RedditPostData if p.postType == IMAGE => p.imageHash
      case p @ _                                    => p.id
    }.toVector

  private def findNewPosts(
    posts: List[RedditPostData]
  )(implicit r: RedisClient, hasher: AverageHash): IO[Vector[RedditPostData]] = IO {
    val newPosts = for {
      postsVec   <- Some(posts.toVector)
      _          <- addSetRedis(postsVec.map(_.id), "reddit:new_posts", r)
      newPostIds <- r.sdiff("reddit:new_posts", "reddit:posts").map(_.toVector.mapFilter(a => a))

      _ = println(s"${LocalDateTime.now()}: Found ${newPostIds.length} new posts")

      newPosts      <- Some(postsVec.filter(p => newPostIds.contains(p.id)))
      postsWithHash <- Some(filterDistinctHash(newPosts.map(_.attachImageHash)))

      _ = println(s"${LocalDateTime.now()}: Hashed ${postsWithHash.length} images")

      _         <- addSetRedis(postsWithHash.mapFilter(_.imageHash), "reddit:new_images", r)
      newImages <- r.sdiff("reddit:new_images", "reddit:images").map(_.toVector.mapFilter(a => a))
      oldImagesPosts <- Some(
                         postsWithHash.mapFilter { p =>
                           p.imageHash
                             .map(!newImages.contains(_))
                             .flatMap(if (_) Some(p.id) else None)
                         }
                       )
      _ <- addSetRedis(oldImagesPosts, "reddit:posts", r)
      _ <- r.del("reddit:new_images", "reddit:new_posts")
      finalPosts <- Some(postsWithHash.filter {
                     case p: RedditPostData if p.postType == IMAGE =>
                       newImages.contains(p.imageHash.getOrElse("z"))
                     case _ => true
                   })
      _ = println(s"${LocalDateTime.now()}: Sending ${finalPosts.length} messages")
    } yield finalPosts

    newPosts.getOrElse(Vector.empty[RedditPostData])
  }

  private val tryConvertInt = (s: String) =>
    IO(s.toIntOption.toRight[String]("Could not convert a key to Int"))

  private def getSubredditsM(r: RedisClient): IO[Vector[Subreddit]] = IO {
    r.hgetall1[String, String]("reddit:subreddits")
      .map(_.toVector.map { case (n, f) => Subreddit(n, f) })
      .getOrElse(Vector.empty[Subreddit])
  }

  private def prepareChInsert(
    ch: Clickhouse,
    values: Seq[AnalyticsEvent]
  ) =
    Uri
      .fromString(ch.host)
      .map { u =>
        POST(
          s"INSERT INTO ${ch.table} FORMAT JSONEachRow\n" ++ values
            .map(_.toJsonLine)
            .mkString("\n"),
          u,
          Authorization(BasicCredentials(ch.user, ch.pass))
        )
      }
      .toOption

  private def sendAnalytics(
    ch: Clickhouse,
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
        .map(_.foldK.filter(_.postType != OTHER))
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
      chHost  <- tryFindKey("CH_HOST", Some("localhost"))
      chUser  <- tryFindKey("CH_USER", Some("default"))
      chPass  <- tryFindKey("CH_PASS", Some(""))
      chTable <- tryFindKey("CH_TABLE", Some("reddit_tg"))
    } yield Clickhouse(chHost, chUser, chPass, chTable)

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
      case Left(exc) =>
        IO.delay(println(s"${LocalDateTime.now()}: $exc")) >> IO.pure(ExitCode.Success)
    }
  }
}
