package models

import play.api.libs.json.JsValue
import play.api.libs.json.Json
import play.api.libs.json.JsArray
import controllers.AppContext

class Comment(
  val id:           Long,
  val postId:       Long,
  val ownerId:      Long,
  val parentId:     Option[Long],
  val content:      String,
  val contentType:  Int,
  val created:      Long,
  val likesCount:   Int,
  val rewardToken:  Long,
  val rewardPower:  Long,
  val rewardDollar: Long,
  val status:       Int) extends TraitDateSupports {

  var parentOpt: Option[Comment] = None

  var ownerOpt: Option[Account] = None

  var nested: Seq[Comment] = Seq()

  var likes: Seq[Like] = Seq()

  var likedOpt: Option[Boolean] = None

  val createdDate = formattedShortDate(created)

  def rewardsToJson(implicit ac: AppContext) = new Reward(Some(rewardPower)).toJson

  def buildTree(comments: Seq[Comment]) {
    nested = comments.filter(_.parentId.fold(false)(_ == id)).sortBy(_.id).reverse
    nested.foreach { child =>
      child.parentOpt = Some(this)
      child.buildTree(comments)
    }
  }

  def toJson(implicit ac: AppContext): JsValue = {
    var jsObj = Json.obj(
      "id" -> id,
      "post_id" -> postId,
      "owner_id" -> ownerId,
      "parent_id" -> parentId,
      "content" -> content,
      "content_type" -> ContentType.strById(contentType),
      "created" -> created,
      "likes_count" -> likesCount,
      "status" -> CommentStatus.strById(status))
    jsObj = jsObj ++ rewardsToJson
    jsObj = ownerOpt.fold(jsObj)(user => jsObj + ("owner" -> user.toJson))
    jsObj = likedOpt.fold(jsObj)(liked => jsObj ++ Json.obj("liked" -> liked))
    jsObj = if (likes.nonEmpty) jsObj + ("likes" -> JsArray(likes.map(_.toJson))) else jsObj
    if (nested.nonEmpty) jsObj + ("nested" -> JsArray(nested.map(_.toJson))) else jsObj
  }

}

object CommentStatus {

  val VISIBLE = 0

  val HIDDEN = 1

  def idByStr(str: String): Option[Int] =
    str match {
      case "visible" => Some(VISIBLE)
      case "hidden"  => Some(HIDDEN)
      case _         => None
    }

  def strById(id: Int): Option[String] =
    id match {
      case 0 => Some("visible")
      case 1 => Some("hidden")
      case _ => None
    }

}

object Comments {

  def buildTree(comments: Seq[Comment]): Seq[Comment] = {
    val ids = comments.map(_.id)
    val rootComments = comments.filter(c => c.parentId.fold(true)(!ids.contains(_)))
    rootComments.foreach(_ buildTree comments)
    rootComments.sortBy(_.id).reverse
  }

}