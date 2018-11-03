package models

import org.joda.time.DateTime
import org.joda.time.DateTimeZone
import org.joda.time.LocalDateTime

import play.api.libs.json.JsValue
import play.api.libs.json.Json
import play.api.libs.json.JsObject
import controllers.AppConstants
import controllers.AppContext
import java.util.Date

class Account(
  val id:                     Long,
  val login:                  String,
  val email:                  String,
  val hash:                   Option[String],
  val avatar:                 Option[String],
  val background:             Option[String],
  val userStatus:             Int,
  val accountStatus:          Int,
  val name:                   Option[String],
  val surname:                Option[String],
  val platformEth:            Option[String],
  val timezoneId:             Int,
  val registered:             Long,
  val confirmCode:            Option[String],
  val postsCounter:           Int,
  val postsCounterStarted:    Long,
  val likesCounter:           Int,
  val likesCounterStarted:    Long,
  val commentsCounter:        Int,
  val commentsCounterStarted: Long,
  val postsCount:             Long,
  val about:                  Option[String],
  val isShowBrand:            Boolean,
  val ITN:                    Option[String],
  val IEC:                    Option[String],
  val phones:                 Option[String],
  val gender:                 Option[String],
  val age:                    Option[Int],
  val rate:                   Int,
  val userType:               Int,
  val sex:                    Option[Int],
  val birthday:               Option[Long],
  val education:              Option[String],
  val bought:                 Int) {

  var balanceTokenOpt: Option[Long] = None

  var balanceDollarOpt: Option[Long] = None

  var balancePowerOpt: Option[Long] = None

  var balances: Map[String, Balance] = Map()

  var sessionOpt: Option[Session] = None

  var roles: Seq[Int] = Seq()

  val ldt = new LocalDateTime(registered, DateTimeZone.UTC)

  val commentsLimt = AppConstants.COMMENTS_COUNTER_LIMIT - commentsCounter

  val likesLimt = AppConstants.LIKES_COUNTER_LIMIT - likesCounter

  val postsLimt = AppConstants.POSTS_COUNTER_LIMIT - postsCounter

  lazy val createdPrettyTime = ContentCompilerHelper.prettyTime.format(new Date(registered))

  def loginMatchedBy(filterOpt: Option[String]): String = 
    filterOpt.fold(login) { filter =>
      val start = login.indexOf(filter)
      val end = start + filter.length;
      val s = "<strong>"
      val e = "</strong>"
      if(start == 0 && end == login.length) {
        s + login + e
      } else if(start == 0 && end != login.length) {
        s + login.substring(0, end) + e + login.substring(end, login.length)
      } else if(start != 0 && end == login.length) {
        login.substring(0, start) + s + login.substring(start, login.length) + e
      } else {
        login.substring(0, start) + s + login.substring(start, end) + e + login.substring(end, login.length)
      }
    }

  override def equals(obj: Any) = obj match {
    case user: Account => user.email == email
    case _             => false
  }

  override def toString = email

  def getRegistered(zone: String): DateTime = getRegistered.toDateTime(DateTimeZone forID zone)

  def getRegistered: LocalDateTime = ldt

  def toJsonAuth(inJsObj: JsObject)(implicit ac: AppContext): JsObject = {
    var jsObj = inJsObj ++ Json.obj("email" -> email)
    userType match {
      case AccountType.USER =>
        jsObj = jsObj ++ Json.obj("name" -> name, "surname" -> surname)
        jsObj = sex.fold(jsObj) { t => jsObj ++ Json.obj("sex" -> t) }
        jsObj = birthday.fold(jsObj) { t => jsObj ++ Json.obj("birthday" -> t) }
        jsObj = education.fold(jsObj) { t => jsObj ++ Json.obj("education" -> t) }
      case AccountType.COMPANY =>
        jsObj = jsObj ++ Json.obj("company_name" -> name)
      case _ =>
    }
    jsObj = confirmCode.fold(jsObj) { t => jsObj ++ Json.obj("confirm_code" -> t) }
    jsObj
  }

  lazy val displayName = userType match {
    case AccountType.COMPANY => name.getOrElse("")
    case _                   => login
  }

  def toJson(implicit ac: AppContext): JsObject = {
    var jsObj = Json.obj(
      "id" -> id,
      "login" -> login,
      "user_status" -> UserStatus.strById(userStatus),
      "account_status" -> AccountStatus.strById(accountStatus),
      "posts_count" -> postsCount,
      "registered" -> registered,
      "comments_limit" -> commentsLimt,
      "likes_limit" -> likesLimt,
      "is_show_brand" -> isShowBrand,
      "bought" -> bought,
      "user_type" -> AccountType.strById(userType),
      "posts_limit" -> postsLimt,
      "display_name" -> displayName,
      "rate" -> rate)

    jsObj = balanceTokenOpt.fold(jsObj) { t => jsObj ++ Json.obj("balance_token" -> t) }
    jsObj = balanceDollarOpt.fold(jsObj) { t => jsObj ++ Json.obj("balance_dollar" -> t) }
    jsObj = balancePowerOpt.fold(jsObj) { t => jsObj ++ Json.obj("balance_power" -> t) }

    jsObj = about.fold(jsObj) { t => jsObj ++ Json.obj("about" -> t) }

    if (userType == AccountType.COMPANY) {
      jsObj = ITN.fold(jsObj) { t => jsObj ++ Json.obj("ITN" -> t) }
      jsObj = IEC.fold(jsObj) { t => jsObj ++ Json.obj("IEC" -> t) }
    }

    jsObj = about.fold(jsObj) { t => jsObj ++ Json.obj("about" -> t) }
    jsObj = avatar.fold(jsObj) { t => jsObj ++ Json.obj("avatar" -> t) }
    jsObj = background.fold(jsObj) { t => jsObj ++ Json.obj("background" -> t) }

    ac.authorizedOpt.fold(jsObj)(_ => toJsonAuth(jsObj))
  }

}


