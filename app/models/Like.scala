package models

import play.api.libs.json.JsValue
import play.api.libs.json.Json
import controllers.AppContext

class Like(
  val id:         Long,
  val ownerId:    Long,
  val targetType: Int,
  val targetId:   Long,
  val created:    Long) extends TraitDateSupports {

  var userLoginOpt: Option[String] = None
  
  var displayNameOpt: Option[String] = None

  var ownerOpt: Option[Account] = None

  var rewardOpt: Option[Reward] = None

  def createdShortDate = formattedShortDate(created)

  def toJson(implicit ac: AppContext): JsValue = {
    var jsObj = Json.obj(
      "id" -> id,
      "owner_id" -> ownerId,
      "target_type" -> TargetType.strById(targetType),
      "target_id" -> targetId,
      "created" -> created)
    jsObj = userLoginOpt.fold(jsObj)(t => jsObj ++ Json.obj("login" -> t))
    jsObj = displayNameOpt.fold(jsObj)(t => jsObj ++ Json.obj("display_name" -> t))
    jsObj = ownerOpt.fold(jsObj)(user => jsObj + ("owner" -> user.toJson))
    jsObj = rewardOpt.fold(jsObj)(t => jsObj ++ t.toJson)
    jsObj
  }

}
