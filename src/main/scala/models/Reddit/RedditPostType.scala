package models.Reddit

sealed abstract class RedditPostType {
  val method: String = "sendMessage"
  val textFieldName: String = "text"
}

case object OTHER extends RedditPostType
case object VIDEO extends RedditPostType
case object GIF extends RedditPostType
case object IMAGE extends RedditPostType {
  override val method: String = "sendPhoto"
  override val textFieldName = "caption"
}

object RedditPostType {
  private val imgFormats = Vector(".jpg", ".png", ".jpeg")
  private val videoHosts = Vector("youtu", "v.redd.it", "gfycat.com")

  def apply(postHint: Option[String], url: String): RedditPostType =
    postHint.map(_.split(':')) match {
      case _ if url.endsWith(".gif")                           => GIF
      case Some(Array("image"))                                => IMAGE
      case Some(Array(_, "video"))                             => VIDEO
      case None if imgFormats.map(url.endsWith).reduce(_ || _) => IMAGE
      case None if videoHosts.map(url.contains).reduce(_ || _) => VIDEO
      case _                                                   => OTHER
    }
}
