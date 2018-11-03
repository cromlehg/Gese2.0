package models

object Roles {

  val CLIENT = 0

  val ADMIN = 1

  def idByStr(str: String): Option[Int] =
    str match {
      case "client" => Some(CLIENT)
      case "admin"  => Some(ADMIN)
      case _        => None
    }

  def strById(id: Int): Option[String] =
    id match {
      case 0 => Some("client")
      case 1 => Some("admin")
      case _ => None
    }

}