package models

object CommentsViewType {

  val LIST = 0

  val TREE = 1

  def idByStr(str: String): Option[Int] =
    str match {
      case "list" => Some(LIST)
      case "tree" => Some(TREE)
      case _      => None
    }

  def strById(id: Int): Option[String] =
    id match {
      case 0 => Some("list")
      case 1 => Some("tree")
      case _ => None
    }

}
