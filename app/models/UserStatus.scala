package models

object UserStatus {

  val NORMAL = 0

  val LOCKED = 1

  def idByStr(str: String): Option[Int] =
    str match {
      case "normal" => Some(NORMAL)
      case "locked" => Some(LOCKED)
      case _        => None
    }

  def strById(id: Int): Option[String] =
    id match {
      case 0 => Some("normal")
      case 1 => Some("locked")
      case _ => None
    }

}