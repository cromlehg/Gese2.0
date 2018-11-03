package models

import play.api.libs.json.JsValue
import play.api.libs.json.Json
import controllers.AppContext

class Session(
  val id:         Long,
  val userId:     Long,
  val ip:         String,
  val sessionKey: String,
  val created:    Long,
  val expire:     Long) extends TraitDateSupports {

  def toJson(implicit ac: AppContext): JsValue = Json.obj()

}

    