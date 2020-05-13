package models

import cats.effect.IO
import io.circe.Json
import org.http4s.client.Client
import org.http4s.circe._

sealed abstract class Feed
case object New extends Feed {
  override def toString: String = "new"
}
case object Hot extends Feed {
  override def toString: String = "hot"
}

case class Subreddit(name: String, feed: Feed) {
  def makeRequestF(client: Client[IO]): IO[Json] =
    client.get(s"https://www.reddit.com/r/$name/$feed.json")(_.as[Json])
}

object Subreddit {
  def apply(name: String, feed: String): Subreddit = feed match {
    case "hot" => Subreddit(name, Hot)
    case _     => Subreddit(name, New)
  }
}
