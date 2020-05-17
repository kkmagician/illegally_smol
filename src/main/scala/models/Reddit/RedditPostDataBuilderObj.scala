package models.Reddit

import java.time.{LocalDateTime, ZoneOffset}

import cats.implicits._
import io.circe.Json
import io.circe.generic.extras.{Configuration, ConfiguredJsonCodec}

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
    postHint: Option[String],
    media: Option[Json],
    linkFlairText: Option[String]
  ) {
    def toData: List[RedditPostData] =
      crosspostParentList match {
        case Some(jsonList) =>
          jsonList
            .mapFilter(_.as[RedditPostDataBuilder].toOption)
            .map(
              _.copy(crosspostParentList = None).toData
                .map(_.copy(crossPostLength = jsonList.length))
            )
            .foldK
        case _ =>
          val mediaUrl = postHint match {
            case Some("hosted:video") | None =>
              media.flatMap { j =>
                j.hcursor.downField("reddit_video").downField("fallback_url").as[String].toOption
              }.getOrElse(url)
            case _ => url
          }

          List(
            RedditPostData(
              id,
              subreddit,
              title,
              LocalDateTime.ofEpochSecond(createdUtc.toLong, 0, ZoneOffset.UTC),
              permalink,
              mediaUrl,
              RedditPostType(postHint, mediaUrl),
              postHint,
              linkFlairText,
              crosspostParentList.map(_.length).getOrElse(0)
            )
          )
      }
  }
}
