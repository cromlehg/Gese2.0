package models

class ScheduledTask(
  val id:        Long,
  val executed:  Option[Long],
  val taskType:  Int,
  val planned:   Option[Long],
  val accountId: Option[Long],
  val productId: Option[Long]) {

  var productOpt: Option[Post] = None

  var emailOpt: Option[String] = None

}
