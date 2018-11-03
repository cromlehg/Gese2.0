package models

import controllers.AppContext
import play.api.libs.json.JsObject
import play.api.libs.json.Json

class MarketingCampaign(
  val id:           Long,
  val productId:    Long,
  val initialCount: Int,
  val count:        Int,
  val price:        Long,
  val start:        Long,
  val end:          Long,
  val status:       Int,
  val descr:        Option[String],
  val title:        String) {

  var productNameOpt: Option[String] = None

  def toJson()(implicit ac: AppContext): JsObject = {
    var jsObj = Json.obj(
      "id" -> id,
      "product_id" -> productId,
      "initalCount" -> initialCount,
      "count" -> count,
      "price" -> price,
      "start" -> start,
      "end" -> end,
      "status" -> MarketingCampaignStatus.strById(status),
      "title" -> title)

    jsObj = productNameOpt.fold(jsObj)(t => jsObj ++ Json.obj("product_name" -> t))
    jsObj = descr.fold(jsObj) { t => jsObj ++ Json.obj("descr" -> t) }
    jsObj
  }

}

object MarketingCampaignStatus {

  val ACTIVE = 0

  val STOPPED = 1

  def idByStr(str: String): Option[Int] =
    str match {
      case "active"  => Some(ACTIVE)
      case "stopped" => Some(STOPPED)
      case _         => None
    }

  def strById(id: Int): Option[String] =
    id match {
      case 0 => Some("active")
      case 1 => Some("stopped")
      case _ => None
    }

}