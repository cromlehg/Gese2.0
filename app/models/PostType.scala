package models

object PostType {

  val ARTICLE = 0

  val REVIEW = 1

  val PRODUCT = 2

  def idByStr(str: String): Option[Int] =
    str match {
      case "review"  => Some(REVIEW)
      case "article" => Some(ARTICLE)
      case "product" => Some(PRODUCT)
      case _         => None
    }

  def strById(id: Int): Option[String] =
    id match {
      case 0 => Some("article")
      case 1 => Some("review")
      case 2 => Some("product")
      case _ => None
    }

}
