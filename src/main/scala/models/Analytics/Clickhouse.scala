package models.Analytics

import cats.data.EitherT
import cats.implicits._
import cats.effect.IO
import models.Reddit.Subreddit
import org.http4s.Method._
import org.http4s.client.Client
import org.http4s.headers.Authorization
import org.http4s.{BasicCredentials, Request, Uri}
import org.http4s.client.dsl.io._

case class Clickhouse(host: String, user: String, pass: String, table: String, setTable: String)

object Clickhouse {
  val makeChAnalyticsQuery: (Seq[AnalyticsEvent], String) => String =
    (values: Seq[AnalyticsEvent], table: String) =>
      s"INSERT INTO $table FORMAT JSONEachRow\n" ++ values.map(_.toJsonLine).mkString("\n")

  val makeSubredditsSetQuery: (Seq[Subreddit], String) => String =
    (subreddits: Seq[Subreddit], table: String) =>
      s"INSERT INTO $table VALUES " ++ subreddits.map(r => s"('${r.name}')").mkString

  def prepareChInsert(
    ch: Clickhouse,
    request: String,
  ): Option[IO[Request[IO]]] =
    Uri
      .fromString(ch.host)
      .map { host =>
        val auth = Authorization(BasicCredentials(ch.user, ch.pass))
        POST(request, host, auth)
      }
      .toOption

  def sendAnalytics(
    ch: Clickhouse,
    query: String,
    client: Client[IO]
  ): EitherT[IO, String, String] =
    prepareChInsert(ch, query) match {
      case Some(req) =>
        client.expect[String](req).attemptT.leftMap(_.getMessage)
      case None => EitherT.leftT[IO, String]("Wrong CH Host")
    }
}
