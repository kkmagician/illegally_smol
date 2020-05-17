package models.Reddit

import cats.effect.IO
import io.circe.Json
import org.http4s.circe._
import org.http4s.client.Client

sealed abstract class Feed
object Feed {
  case object `new` extends Feed
  case object hot extends Feed
}

case class Subreddit(name: String, feed: Feed) {
  def makeRequestF(client: Client[IO]): IO[Json] =
    client.get(s"https://www.reddit.com/r/$name/$feed.json")(_.as[Json])
}

object Subreddit {
  def apply(name: String, feed: String): Subreddit = feed match {
    case "hot" => Subreddit(name, Feed.hot)
    case _     => Subreddit(name, Feed.`new`)
  }
}
