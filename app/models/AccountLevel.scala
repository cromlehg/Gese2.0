package models

object AccountLevel {

  val MAX_K = 1.4

  def kByRate(rate: Int): Double =
    rate match {
      case 1 => 1.1
      case 2 => 1.2
      case 3 => 1.3
      case 4 => MAX_K
      case _ => 1
    }

  def getRateByBoughtCount(count: Int) =
    if (count < 10)
      0
    else if (count < 50)
      1
    else if (count < 250)
      2
    else if (count < 1250)
      3
    else
      4

}