package models

object AccountStatus {

  val CONFIRMED = 0

  val WAITE_CONFIRMATION = 1

  def idByStr(str: String): Option[Int] =
    str match {
      case "confirmed"          => Some(CONFIRMED)
      case "waite confirmation" => Some(WAITE_CONFIRMATION)
      case _                    => None
    }

  def strById(id: Int): Option[String] =
    id match {
      case 0 => Some("confirmed")
      case 1 => Some("waite confirmation")
      case _ => None
    }

}

