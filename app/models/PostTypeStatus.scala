package models

object PostTypeStatus {

  val ACTIVE = 0

  def idByStr(str: String): Option[Int] =
    str match {
      case "active" => Some(ACTIVE)
      case _        => None
    }

  def strById(id: Int): Option[String] =
    id match {
      case 0 => Some("active")
      case _ => None
    }

}
