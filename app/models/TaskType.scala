package models

object TaskType {

  val REWARDER = 1

  val SCHEDULED_TXS_PROCESSOR = 2

  val NOTIFICATIONS = 3

  def idByStr(str: String): Option[Int] =
    str match {
      case "rewarder"                => Some(REWARDER)
      case "scheduled txs processor" => Some(SCHEDULED_TXS_PROCESSOR)
      case "notifications"           => Some(NOTIFICATIONS)
      case _                         => None
    }

  def strById(id: Int): Option[String] =
    id match {
      case 1 => Some("rewarder")
      case 2 => Some("scheduled txs processor")
      case 3 => Some("notifications")
      case _ => None
    }

}