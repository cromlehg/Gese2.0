package models

import controllers.AppContext
import play.api.libs.json.JsObject
import play.api.libs.json.Json

class Balance(
  val id:          Long,
  val ownerId:     Long,
  val ownerType:   Int,
  val currencyId:  Long,
  val updated:     Long,
  val balanceType: Int,
  val value:       Long) {

  var currencyOpt: Option[Currency] = None

  def toJson()(implicit ac: AppContext): JsObject = {
    var jsObj = Json.obj(
      "id" -> id,
      "owner_id" -> ownerId,
      "owner_type" -> TargetType.strById(ownerType),
      "currency" -> CurrencyType.strById(currencyId.toInt),
      "updated" -> updated,
      "balance_type" -> BalanceType.strById(balanceType),
      "value" -> value)
    jsObj
  }
}