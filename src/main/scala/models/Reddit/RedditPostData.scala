package models.Reddit

import java.net.URL
import java.time.{LocalDateTime, ZoneOffset}

import javax.imageio.ImageIO
import cats.effect.IO
import cats.implicits._
import com.github.kilianB.hashAlgorithms.AverageHash
import com.redis.RedisClient
import io.circe.Json
import models.Analytics._
import models.Telegram.TelegramResponseObj.TelegramResponse
import io.circe.generic.auto._
import org.http4s.Method._
import org.http4s.circe._
import org.http4s.client.Client
import org.http4s.{Uri, UrlForm}
import org.http4s.client.dsl.io._

import scala.util.Try

case class RedditPostData(
  id: String,
  subreddit: String,
  title: String,
  created: LocalDateTime,
  permalink: String,
  url: String,
  postType: RedditPostType,
  nativePostType: Option[String],
  flair: Option[String],
  crossPostLength: Int,
  imageHash: Option[String] = None
) {
  val toMessage: String = {
    val flairPart = flair match {
      case Some(f) => s"\n<i>$f</i>"
      case None    => ""
    }

    val base = s"$title$flairPart"
    val attachLink = (base: String, name: String) => base ++ "\n" ++ s"""<a href="$url">$name</a>"""

    postType match {
      case VIDEO => attachLink(base, "video")
      case GIF   => attachLink(base, "gif")
      case _     => base
    }
  }

  def attachImageHash(implicit hasher: AverageHash): RedditPostData = postType match {
    case IMAGE | GIF =>
      val imgHash = for {
        img  <- Try(ImageIO.read(new URL(url)))
        hash <- Try(hasher.hash(img).getHashValue.toString)
      } yield hash
      this.copy(imageHash = imgHash.toOption)
    case _ => this
  }

  val isUnwanted: Boolean = postType match {
    case IMAGE | GIF if imageHash.isDefined => true
    case VIDEO                              => true
    case _                                  => false
  }

  def storeKeys(r: RedisClient): IO[Unit] =
    for {
      _ <- IO(r.sadd("reddit:posts", id))
      _ <- IO(imageHash.map(r.sadd("reddit:images", _)))
    } yield println(s"Sent & stored ID $id")

  def sendMessage(botToken: String, chat: String)(
    client: Client[IO],
    r: RedisClient
  ): IO[AnalyticsEvent] = {
    val msg = UrlForm(
      "chat_id" -> chat,
      postType.textFieldName -> this.toMessage,
      "parse_mode" -> "HTML"
    ) +? ("photo", postType match {
      case IMAGE => Some(url)
      case _     => None
    })

    val jsonResponse = Uri
      .fromString(s"https://api.telegram.org/bot$botToken/${postType.method}")
      .map(uri => POST(msg, uri))
      .map(req => {
        client.fetchAs[TelegramResponse](req)(jsonOf[IO, TelegramResponse])
      })

    jsonResponse match {
      case Right(resp: IO[TelegramResponse]) =>
        for {
          tgResp <- resp
          _      <- storeKeys(r)
          analytics <- IO.pure(
                        AnalyticsEventOk(
                          tgResp.result.chat.id,
                          tgResp.result.messageId,
                          id,
                          subreddit,
                          title,
                          created,
                          LocalDateTime.ofEpochSecond(tgResp.result.date, 0, ZoneOffset.UTC),
                          url,
                          postType.toString,
                          nativePostType,
                          flair,
                          imageHash.flatMap(_.toIntOption)
                        )
                      )
        } yield analytics
      case Left(e) =>
        IO.pure(AnalyticsEventError(e.getMessage))
    }
  }
}
