package models

object TxType {

  val USER_TO_USER = 0

  val LIKER_REWARD = 1

  val LIKE_REWARD = 2

  val COMMENTER_REWARD = 3

  val PROMOTE_POST = 4

  val POSTER_REWARD = 5

  val REGISTER_REWARD = 6

  val EXCHANGE_FROM = 7

  val EXCHANGE_TO = 8

  val CREATE_CAMPAIGN_DEPOSIT = 9

  val CREATE_BATCH_DEPOSIT = 10

  val REDEEM_REWARD_FROM_BATCH = 11

  val REDEEM_REWARD_FROM_CAMPAING = 12

  val CHARGE = 13

  def idByStr(str: String): Option[Int] =
    str match {
      case "user to user"                => Some(USER_TO_USER)
      case "liker reward"                => Some(LIKER_REWARD)
      case "like reward"                 => Some(LIKE_REWARD)
      case "commenter reward"            => Some(COMMENTER_REWARD)
      case "promote post"                => Some(PROMOTE_POST)
      case "poster reward"               => Some(POSTER_REWARD)
      case "register reward"             => Some(REGISTER_REWARD)
      case "exchange from"               => Some(EXCHANGE_FROM)
      case "exchange to"                 => Some(EXCHANGE_TO)
      case "create campaign deposit"     => Some(CREATE_CAMPAIGN_DEPOSIT)
      case "create batch deposit"        => Some(CREATE_BATCH_DEPOSIT)
      case "redeem reward from batch"    => Some(REDEEM_REWARD_FROM_BATCH)
      case "redeem reward from campaing" => Some(REDEEM_REWARD_FROM_CAMPAING)
      case "charge"                      => Some(CHARGE)
      case _                             => None
    }

  def strById(id: Int): Option[String] =
    id match {
      case 0  => Some("user to user")
      case 1  => Some("liker reward")
      case 2  => Some("like reward")
      case 3  => Some("commenter reward")
      case 4  => Some("promote post")
      case 5  => Some("poster reward")
      case 6  => Some("register reward")
      case 7  => Some("exchange from")
      case 8  => Some("exchange to")
      case 9  => Some("create campaign deposit")
      case 10 => Some("create batch deposit")
      case 11 => Some("redeem reward from batch")
      case 12 => Some("redeem reward from campaing")
      case 13 => Some("charge")
      case _  => None
    }

}
