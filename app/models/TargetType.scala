package models

object TargetType {

  val SYSTEM = 0

  val ACCOUNT = 1

  val POST = 2

  val REVIEW = 3

  val ARTICLE = 4

  val COMMENT = 5

  val ETH = 6

  val PRODUCT = 7

  val CAMPAIGN = 8

  val BATCH = 9

  def idByStr(str: String): Option[Int] =
    str match {
      case "system"   => Some(SYSTEM)
      case "account"  => Some(ACCOUNT)
      case "post"     => Some(POST)
      case "review"   => Some(REVIEW)
      case "article"  => Some(ARTICLE)
      case "comment"  => Some(COMMENT)
      case "eth"      => Some(ETH)
      case "product"  => Some(PRODUCT)
      case "campaign" => Some(CAMPAIGN)
      case "batch"    => Some(BATCH)
      case _          => None
    }

  def strById(id: Int): Option[String] =
    id match {
      case 0 => Some("system")
      case 1 => Some("account")
      case 2 => Some("post")
      case 3 => Some("review")
      case 4 => Some("article")
      case 5 => Some("comment")
      case 6 => Some("eth")
      case 7 => Some("product")
      case 8 => Some("campaign")
      case 9 => Some("batch")
      case _ => None
    }

}