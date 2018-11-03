package models

import controllers.AppContext
import play.api.libs.json.JsValue
import play.api.libs.json.Json

class Batch(
  val id:        Long,
  val productId: Long,
  val created:   Long,
  val count:     Int,
  val price:     Long) {

  var balanceOpt: Option[Balance] = None

  var ownerOpt: Option[Account] = None

  var productOpt: Option[Post] = None

  var productNameOpt: Option[String] = None

  def toJson(implicit ac: AppContext): JsValue = {
    var jsObj = Json.obj(
      "id" -> id,
      "product_id" -> productId,
      "created" -> created,
      "count" -> count,
      "price" -> price)
    jsObj = productNameOpt.fold(jsObj)(t => jsObj ++ Json.obj("product_name" -> t))
    jsObj = productOpt.fold(jsObj)(t => jsObj + ("product" -> t.toJson))
    jsObj = ownerOpt.fold(jsObj)(t => jsObj + ("owner" -> t.toJson))
    jsObj = balanceOpt.fold(jsObj)(t => jsObj ++ Json.obj("balance" -> t.value))
    jsObj
  }

}