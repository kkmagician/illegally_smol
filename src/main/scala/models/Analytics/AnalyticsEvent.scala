package models.Analytics

import java.time.LocalDateTime

import io.circe.generic.auto._
import io.circe.syntax._

trait AnalyticsEvent {
  def toJsonLine: String
}

case class AnalyticsEventOk(
  chatId: Long,
  messageId: Int,
  postId: String,
  subreddit: String,
  title: String,
  postCreatedTime: LocalDateTime,
  messageSentTime: LocalDateTime,
  url: String,
  postType: String,
  nativePostType: Option[String],
  flair: Option[String],
  imageHash: Option[Int]
) extends AnalyticsEvent {
  def toJsonLine: String = this.asJson.dropNullValues.noSpaces
}

case class AnalyticsEventError(message: String, isError: Int = 1) extends AnalyticsEvent {
  def toJsonLine: String = this.asJson.dropNullValues.noSpaces
}
