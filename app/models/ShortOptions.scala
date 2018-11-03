package models

class ShortOption(
  val id:    Long,
  val name:  String,
  val descr: String,
  val ttype: String,
  val value: String) {

  def toBoolean = value.toBoolean

}

object ShortOptions {

  val TYPE_BOOLEAN = "Boolean"

  val ARTICLE_ALLOWED = "ARTICLE_ALLOWED"

  val REVIEWS_ALLOWED = "REVIEWS_ALLOWED"

  val PRODUCTS_ALLOWED = "PRODUCTS_ALLOWED"
  
  val PRODUCTS_PREMODERATION = "PRODUCTS_PREMODERATION"
  
  val ARTICLES_PREMODERATION = "ARTICLES_PREMODERATION"
  
  val REVIEWS_PREMODERATION = "REVIEWS_PREMODERATION"

}