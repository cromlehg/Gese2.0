package models

object RewardType {

  val POWER_50_DOLLARS_50 = 0

  val POWER = 1

  val DOLLAR = 2

  def idByStr(str: String): Option[Int] =
    str match {
      case "50 power/50 dollars" => Some(POWER_50_DOLLARS_50)
      case "power"               => Some(POWER)
      case "dollar"              => Some(DOLLAR)
      case _                     => None
    }

  def strById(id: Int): Option[String] =
    id match {
      case 0 => Some("50 power/50 dollars")
      case 1 => Some("power")
      case 2 => Some("dollar")
      case _ => None
    }

}
