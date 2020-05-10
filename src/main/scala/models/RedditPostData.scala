package models

import java.time.LocalDateTime

import cats.effect.{IO, Resource}
import cats.implicits._
import com.redis.RedisClient
import io.circe.{Json, JsonObject}
import org.http4s.circe._
import org.http4s.client.Client
import org.http4s.client.dsl.io._
import org.http4s.Method._
import org.http4s.{Uri, UrlForm}

case class RedditPostData(
  id: String,
  subreddit: String,
  title: String,
  created: LocalDateTime,
  permalink: String,
  url: String,
  postType: RedditPostType,
  flair: Option[String],
  crossPostLength: Int
) {
  val toMessage: String = {
    val flairPart = flair match {
      case Some(f) => s"\n<i>$f</i>"
      case None    => ""
    }

    val base = s"$title\n#$subreddit$flairPart"
    postType match {
      case OTHER => "#other\n" ++ base ++ "\n" ++ s"""<a href="$url">link</a>"""
      case VIDEO => base ++ "\n" ++ s"""<a href="$url">link</a>"""
      case IMAGE => base
    }
  }

  def storeKeys(r: RedisClient): IO[Unit] =
    IO(r.sadd("reddit:posts", id)) >> IO.delay(
      println(s"Sent & stored ID $id")
    )

  def sendMessage(botToken: String, chat: String)(
    client: Resource[IO, Client[IO]],
    r: RedisClient
  ): IO[Json] = {
    val msg = UrlForm(
      "chat_id" -> chat,
      postType.textFieldName -> this.toMessage,
      "parse_mode" -> "HTML"
    ) +? ("photo", postType match {
      case IMAGE => Some(url)
      case _     => None
    })

    client.use { c =>
      val jsonResponse = Uri
        .fromString(
          s"https://api.telegram.org/bot$botToken/${postType.method}"
        )
        .map(uri => POST(msg, uri))
        .map(c.expect[Json](_))

      jsonResponse match {
        case Right(result) =>
          result.flatMap(
            j =>
              j.hcursor.downField("ok").as[Boolean] match {
                case Right(isOk) if isOk => storeKeys(r) >> IO.pure(j)
                case _                   => IO.pure(j)
            }
          )
        case Left(e) =>
          IO.pure(Json.fromString(e.getMessage))
      }
    }
  }
}
