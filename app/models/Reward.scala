package models

import controllers.RewardLogic
import play.api.libs.json.JsObject
import play.api.libs.json.Json
import controllers.AppContext

class Reward(
  rewardPowerOpt:  Option[Long] = None,
  rewardDollarOpt: Option[Long] = None,
  rewardTokenOpt:  Option[Long] = None) {

  lazy val rewardEquvalent = RewardLogic.rewardDollarEquivalent(rewardDollarOpt.getOrElse(0), rewardPowerOpt.getOrElse(0))

  def toJson(implicit ac: AppContext): JsObject = {
    var jsObj = Json.obj()
    jsObj = rewardPowerOpt.fold(jsObj) { t => jsObj ++ Json.obj("reward_power" -> t) }
    jsObj = rewardDollarOpt.fold(jsObj) { t => jsObj ++ Json.obj("reward_dollar" -> t) }
    jsObj = rewardTokenOpt.fold(jsObj) { t => jsObj ++ Json.obj("reward_token" -> t) }
    jsObj = jsObj ++ Json.obj("reward_dollar_equivalent" -> rewardEquvalent)
    jsObj
  }

}