package models

object CurrencyType {

  val TOKEN = 1

  val POWER = 2

  val DOLLAR = 3

  val ETH = 4

  def idByStr(str: String): Option[Int] =
    str match {
      case "token"  => Some(TOKEN)
      case "power"  => Some(POWER)
      case "dollar" => Some(DOLLAR)
      case "eth"    => Some(ETH)
      case _        => None
    }

  def strById(id: Int): Option[String] =
    id match {
      case 1 => Some("token")
      case 2 => Some("power")
      case 3 => Some("dollar")
      case 4 => Some("eth")
      case _ => None
    }

}
