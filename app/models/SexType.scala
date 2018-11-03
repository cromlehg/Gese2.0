package models

object SexType {

  val MALE = 0

  val FEMALE = 1

  def idByStr(str: String): Option[Int] =
    str match {
      case "male"   => Some(MALE)
      case "female" => Some(FEMALE)
      case _        => None
    }

  def strById(id: Int): Option[String] =
    id match {
      case 0 => Some("male")
      case 1 => Some("female")
      case _ => None
    }

}