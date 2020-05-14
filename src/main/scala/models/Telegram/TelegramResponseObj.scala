package models.Telegram

import io.circe.generic.auto._
import io.circe.generic.extras._

object TelegramResponseObj {
  private implicit val config: Configuration =
    Configuration.default.withSnakeCaseMemberNames

  case class TelegramChat(id: Long, title: String)

  @ConfiguredJsonCodec
  case class TelegramResult(messageId: Int, chat: TelegramChat, date: Int)

  case class TelegramResponse(ok: Boolean, result: TelegramResult)

}
