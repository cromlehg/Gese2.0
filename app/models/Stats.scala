package models

import controllers.AppConstants

class Stats(
  accounts:  Long,
  users:     Long,
  companies: Long,
  tokens:    Long,
  dollars:   Long,
  power:     Long,
  posts:     Long,
  articles:  Long,
  reviews:   Long,
  products:  Long) {

  def format(x: Double): String = if (x == 0) "0" else "%1.2f".format(x)

  val namesToValues = Seq(
    ("accounts", accounts),
    ("users", users),
    ("companies", companies),
    ("tokens", format(tokens.toDouble / AppConstants.DECIMALS.toDouble)),
    ("dollars", format(dollars.toDouble / AppConstants.DECIMALS.toDouble)),
    ("power", format(power.toDouble / AppConstants.DECIMALS.toDouble)),
    ("posts", posts),
    ("articles", articles),
    ("reviews", reviews),
    ("products", products)).map(t => Seq(t._1.toString, t._2.toString))

}