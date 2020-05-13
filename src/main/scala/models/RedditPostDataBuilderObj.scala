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
    media: Option[Json],
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
          val mediaUrl = postHint match {
            case "hosted:video" =>
              media.flatMap { j =>
                j.hcursor.downField("reddit_video").downField("fallback_url").as[String].toOption
              }
            case _ => None
          }

          List(
            RedditPostData(
              id,
              subreddit,
              title,
              LocalDateTime.ofEpochSecond(createdUtc.toLong, 0, ZoneOffset.UTC),
              permalink,
              if (mediaUrl.isDefined) mediaUrl.getOrElse("") else url,
              RedditPostType(postHint),
              linkFlairText,
              crosspostParentList.map(_.length).getOrElse(0)
            )
          )
      }
  }
}
