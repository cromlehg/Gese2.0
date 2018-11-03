package models

import controllers.AppContext
import play.api.libs.json.JsObject
import play.api.libs.json.Json

class Item(
  val id:      Long,
  val batchId: Long,
  val code:    String,
  val status:  Int,
  val bought:  Option[Long],
  val buyerId: Option[Long]) {

  def toJson()(implicit ac: AppContext): JsObject = {
    var jsObj = Json.obj(
      "id" -> id,
      "batch_id" -> batchId,
      "status" -> ItemStatus.strById(status),
      "bought" -> bought,
      "buyer_id" -> buyerId)
    jsObj
  }

}