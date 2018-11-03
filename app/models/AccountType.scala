package models

object AccountType {

  val USER = 0

  val COMPANY = 1

  def idByStr(str: String): Option[Int] =
    str match {
      case "user"    => Some(USER)
      case "company" => Some(COMPANY)
      case _         => None
    }

  def strById(id: Int): Option[String] =
    id match {
      case 0 => Some("user")
      case 1 => Some("company")
      case _ => None
    }

}