package models

object ItemStatus {

  val READY = 0

  val BOUGHT = 1

  def idByStr(str: String): Option[Int] =
    str match {
      case "ready"  => Some(READY)
      case "bought" => Some(BOUGHT)
      case _        => None
    }

  def strById(id: Int): Option[String] =
    id match {
      case 0 => Some("ready")
      case 1 => Some("bought")
      case _ => None
    }

}
