package models

object ModerateStatus {

  val SUCCESS = 0

  val FAILED = 1

  val NEED_A_MODERATION = 2

  def idByStr(str: String): Option[Int] =
    str match {
      case "success" => Some(SUCCESS)
      case "failed" => Some(FAILED)
      case "need a moderation" => Some(NEED_A_MODERATION)
      case _ => None
    }

  def strById(id: Int): Option[String] =
    id match {
      case 0 => Some("success")
      case 1 => Some("failed")
      case 2 => Some("need a moderation")
      case _ => None
    }

}