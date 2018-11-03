package models

object BalanceType {

  val CURRENT = 0

  val PREVIOUS = 1

  def idByStr(str: String): Option[Int] =
    str match {
      case "current"  => Some(CURRENT)
      case "previous" => Some(PREVIOUS)
      case _          => None
    }

  def strById(id: Int): Option[String] =
    id match {
      case 0 => Some("current")
      case 1 => Some("previous")
      case _ => None
    }

}