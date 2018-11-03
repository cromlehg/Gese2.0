package models

object TxState {

  val APPROVED = 0

  val SCHEDULED = 1

  def idByStr(str: String): Option[Int] =
    str match {
      case "approved"  => Some(APPROVED)
      case "scheduled" => Some(SCHEDULED)
      case _           => None
    }

  def strById(id: Int): Option[String] =
    id match {
      case 0 => Some("approved")
      case 1 => Some("scheduled")
      case _ => None
    }

}
