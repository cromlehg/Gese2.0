package models

import java.util.Date

import controllers.AppContext
import play.api.libs.json.JsObject
import play.api.libs.json.Json

class Position(
  val id:        Long,
  val itemId:    Long,
  val timestamp: Long,
  val longitude: Double,
  val latitude:  Double,
  val accuracy:  Double) extends TraitDateSupports {

  val createdShortDate = formattedShortDate(timestamp)

  lazy val createdPrettyTime = ContentCompilerHelper.prettyTime.format(new Date(timestamp))

  def toJson()(implicit ac: AppContext): JsObject = {
    var jsObj = Json.obj(
      "id" -> id,
      "item_id" -> itemId,
      "timestamp" -> timestamp,
      "longitude" -> longitude,
      "latitude" -> latitude,
      "accuracy" -> accuracy)
    jsObj
  }

}

