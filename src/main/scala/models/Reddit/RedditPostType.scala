package models.Reddit

sealed abstract class RedditPostType {
  val method: String = "sendMessage"
  val textFieldName: String = "text"
}
case object VIDEO extends RedditPostType
case object OTHER extends RedditPostType
case object IMAGE extends RedditPostType {
  override val method: String = "sendPhoto"
  override val textFieldName = "caption"
}

object RedditPostType {
  def apply(postHint: String): RedditPostType = postHint.split(':') match {
    case Array("image")    => IMAGE
    case Array(_, "video") => VIDEO
    case _                 => OTHER
  }
}
