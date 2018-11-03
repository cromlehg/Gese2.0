package models

import play.api.libs.json.JsValue
import play.api.libs.json.Json
import controllers.AppContext

class Transaction(
  val id:            Long,
  val created:       Long,
  val scheduled:     Option[Long],
  val processed:     Option[Long],
  val fromType:      Int,
  val toType:        Int,
  val fromId:        Option[Long],
  val toId:          Option[Long],
  val fromRouteType: Option[Int],
  val toRouteType:   Option[Int],
  val fromRouteId:   Option[Long],
  val toRouteId:     Option[Long],
  val from:          Option[String],
  val to:            Option[String],
  val txType:        Int,
  val msg:           Option[String],
  val state:         Int,
  val currencyId:    Int,
  val amount:        Long) extends TraitDateSupports {

  var fromAccountOpt: Option[Account] = None

  var toAccountOpt: Option[Account] = None

  def registerdShortDate = formattedShortDate(created)

  def toJson(implicit ac: AppContext): JsValue = {
    var jsObj = Json.obj(
      "id" -> id,
      "created" -> created,
      "from_type" -> TargetType.strById(fromType),
      "to_type" -> TargetType.strById(toType),
      "tx_type" -> TxType.strById(txType),
      "state" -> TxState.strById(state),
      "currency" -> CurrencyType.strById(currencyId),
      "amount" -> amount)

    jsObj = scheduled.fold(jsObj) { t => jsObj ++ Json.obj("scheduled" -> t) }
    jsObj = processed.fold(jsObj) { t => jsObj ++ Json.obj("processed" -> t) }
    jsObj = fromId.fold(jsObj) { t => jsObj ++ Json.obj("from_id" -> t) }
    jsObj = toId.fold(jsObj) { t => jsObj ++ Json.obj("to_id" -> t) }
    jsObj = fromRouteId.fold(jsObj) { t => jsObj ++ Json.obj("from_route_id" -> t) }
    jsObj = toRouteId.fold(jsObj) { t => jsObj ++ Json.obj("to_route_id" -> t) }
    jsObj = fromRouteType.fold(jsObj) { t => jsObj ++ Json.obj("from_route_type" -> TargetType.strById(t)) }
    jsObj = toRouteType.fold(jsObj) { t => jsObj ++ Json.obj("to_route_type" -> TargetType.strById(t)) }
    jsObj = from.fold(jsObj) { t => jsObj ++ Json.obj("from" -> t) }
    jsObj = to.fold(jsObj) { t => jsObj ++ Json.obj("to" -> t) }
    jsObj = msg.fold(jsObj) { t => jsObj ++ Json.obj("msg" -> t) }

    jsObj = fromAccountOpt.fold(jsObj)(t => jsObj + ("from_user" -> t.toJson))
    jsObj = toAccountOpt.fold(jsObj)(t => jsObj + ("to_user" -> t.toJson))

    jsObj
  }

}
