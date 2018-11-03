package models

object ContentType {

  val TEXT = 0

  val HTML = 2

  val MARKDOWN = 3

  def idByStr(str: String): Option[Int] =
    str match {
      case "text"     => Some(TEXT)
      case "html"     => Some(HTML)
      case "markdown" => Some(MARKDOWN)
      case _          => None
    }

  def strById(id: Int): Option[String] =
    id match {
      case 0 => Some("text")
      case 1 => Some("html")
      case 2 => Some("markdown")
      case _ => None
    }

}
