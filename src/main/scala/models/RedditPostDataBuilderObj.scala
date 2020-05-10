package models

import cats.implicits._
import java.time.{LocalDateTime, ZoneOffset}

import io.circe.Json
import io.circe.generic.extras._

object RedditPostDataBuilderObj {
  private implicit val config: Configuration =
    Configuration.default.withSnakeCaseMemberNames

  @ConfiguredJsonCodec
  case class RedditPostDataBuilder(
    crosspostParentList: Option[List[Json]],
    id: String,
    subreddit: String,
    title: String,
    createdUtc: Float,
    permalink: String,
    url: String,
    postHint: String,
    linkFlairText: Option[String]
  ) {
    def toData: List[RedditPostData] =
      crosspostParentList match {
        case Some(jsonList) =>
          jsonList
            .mapFilter(_.as[RedditPostDataBuilder].toOption)
            .map(_.copy(crosspostParentList = None).toData)
            .foldK
        case _ =>
          List(
            RedditPostData(
              id,
              subreddit,
              title,
              LocalDateTime.ofEpochSecond(createdUtc.toLong, 0, ZoneOffset.UTC),
              permalink,
              url,
              RedditPostType(postHint),
              linkFlairText,
              crosspostParentList.map(_.length).getOrElse(0)
            )
          )
      }
  }
}
