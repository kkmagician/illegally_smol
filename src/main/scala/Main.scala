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

import models.Ops._
import models.Analytics.{AnalyticsEvent, Clickhouse}
import models.Analytics.Clickhouse._
import models.Reddit._
import models.Reddit.RedditPostDataBuilderObj._
import models.Telegram.TelegramCreds

import scala.concurrent.ExecutionContext.global
import scala.concurrent.duration._

object Main extends IOApp {
  private val postsOptic = root.data.children.each.data.json

  private val parsePostsData = (l: List[Json]) =>
    l.map(_.as[RedditPostDataBuilder]).mapFilter(_.toOption.map(_.toData)).foldK

  private def getNewPostsF(
    subreddits: Seq[Subreddit],
    clientResourse: Resource[IO, Client[IO]]
  ): IO[List[RedditPostData]] =
    clientResourse.use { client =>
      subreddits
        .map(_.makeRequestF(client).map(postsOptic.getAll).map(parsePostsData))
        .toVector
        .sequence
        .map(_.foldK)
    }

  private def addSetRedis(add: Vector[Any], key: String, r: RedisClient): Option[Long] = add match {
    case head +: tail if tail.nonEmpty =>
      r.sadd(key, head, tail: _*)
    case head +: _ => r.sadd(key, head)
    case _         => Some(0)
  }

  private val filterDistinctHash = (posts: Seq[RedditPostData]) =>
    posts.distinctBy {
      case p: RedditPostData if List(IMAGE, GIF).contains(p.postType) => p.imageHash
      case p @ _                                                      => p.id
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
                     case p: RedditPostData if List(IMAGE, GIF).contains(p.postType) =>
                       newImages.contains(p.imageHash.getOrElse("z"))
                     case _ => true
                   })
      _ = println(s"${LocalDateTime.now()}: Sending ${finalPosts.length} messages")
    } yield finalPosts

    newPosts.getOrElse(Vector.empty[RedditPostData])
  }

  private val tryConvertInt = (s: String) =>
    IO(s.toIntOption.toRight[String]("Could not convert a key to Int"))

  private def getSubredditsM(r: RedisClient): EitherT[IO, String, Vector[Subreddit]] =
    IO {
      r.hgetall1[String, String]("reddit:subreddits")
        .map(_.toVector.map { case (n, f) => Subreddit(n, f) })
        .getOrElse(Vector.empty[Subreddit])
    }.attemptT.leftMap(_.getMessage)

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

    val makeRun = (subreddits: Vector[Subreddit], r: RedisClient, creds: TelegramCreds) => {
      val posts = for {
        _             <- IO(println(subreddits.mkString(", ")))
        fetchedPosts  <- getNewPostsF(subreddits, client)
        filteredPosts <- IO(fetchedPosts.filter(_.postType != OTHER))
        newPosts      <- findNewPosts(filteredPosts)(r, imageHasher)
      } yield newPosts

      val sendPosts = posts >>= { ps =>
        client.use { c =>
          ps.map {
            _.sendMessage(creds.botToken, creds.chatId)(c, r)
              .thenWait(3.seconds)
              .attemptT
              .leftMap(err => {
                println(err.getMessage)
                err
              })
              .value
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
      chSet   <- tryFindKey("CH_SET", Some("sets.subreddits"))
    } yield Clickhouse(chHost, chUser, chPass, chTable, chSet)

    def analyticsInserts(
      ch: Clickhouse,
      events: Seq[AnalyticsEvent],
      subreddits: Seq[Subreddit],
      clientRes: Resource[IO, Client[IO]]
    ): EitherT[IO, String, String] =
      if (events.nonEmpty) {
        val eventsQuery = makeChAnalyticsQuery(events, ch.table)
        val subredditsSetQuery = makeSubredditsSetQuery(subreddits, ch.setTable)

        val response = clientRes.use { client =>
          val resp = for {
            events <- sendAnalytics(ch, eventsQuery, client)
            set    <- sendAnalytics(ch, subredditsSetQuery, client)
          } yield List(events, set).mkString("\n")

          resp.value
        }
        EitherT(response)
      } else EitherT.rightT[IO, String]("No posts to write to analytics")

    val program = for {
      r          <- redis
      ch         <- clickhouse
      creds      <- telegramCreds
      subreddits <- getSubredditsM(r)
      run        <- makeRun(subreddits, r, creds)
      ch         <- analyticsInserts(ch, run.mapFilter(_.toOption), subreddits, client)
      _ = println(ch)
    } yield run

    program.value >>= {
      case Right(v) =>
        for {
          jsons <- IO.pure(v.mapFilter(_.toOption.map(_.toJsonLine)).mkString("\n"))
          _     <- IO.delay(println(jsons))
        } yield ExitCode.Success
      case Left(exc) =>
        IO.delay(println(s"${LocalDateTime.now()}: $exc")) >> IO.pure(ExitCode.Success)
    }
  }
}
