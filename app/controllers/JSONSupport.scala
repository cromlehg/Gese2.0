package controllers

import scala.concurrent.Future

import play.api.libs.json.JsObject
import play.api.libs.json.JsValue
import play.api.libs.json.Json
import play.api.mvc.AbstractController
import play.api.mvc.Request
import play.api.mvc.Result

trait JSONSupport { self: AbstractController =>

  import scala.concurrent.Future.{ successful => future }

  protected def jsonResult(isSuccess: Boolean, codeOpt: Option[Int], msgOpt: Option[String], jsObjOpt: Option[JsValue]): JsObject = {
    var jsObj = Json.obj("status" -> (if (isSuccess) "ok" else "error"))
    jsObj = codeOpt.fold(jsObj)(code => jsObj ++ Json.obj("code" -> code))
    jsObj = msgOpt.fold(jsObj)(msg => jsObj ++ Json.obj("msg" -> msg))
    jsObj = jsObjOpt.fold(jsObj)(jsObjIn => jsObj + ("result" -> jsObjIn))
    jsObj
  }

  protected def jsonResultError(code: Int, msg: String): JsObject =
    jsonResultError(code, Some(msg))

  protected def jsonResultError(code: Int, msgOpt: Option[String]): JsObject =
    jsonResult(false, Some(code), msgOpt, None)

  protected def jsonResultOk(jsObjOpt: Option[JsValue]): JsObject =
    jsonResult(true, None, None, jsObjOpt)

  protected def jsonResultOk(): JsObject =
    jsonResultOk(None)

  protected def jsonResultOk(jsObj: JsValue): JsObject =
    jsonResultOk(Some(jsObj))

  protected def fieldIntOpt(fieldName: String)(f: Option[Int] => Future[Result])(implicit request: Request[JsValue]): Future[Result] =
    f((request.body \ fieldName).asOpt[Int])

  protected def fieldStringOpt(fieldName: String)(f: Option[String] => Future[Result])(implicit request: Request[JsValue]): Future[Result] =
    f((request.body \ fieldName).asOpt[String])

  protected def fieldLongOpt(fieldName: String)(f: Option[Long] => Future[Result])(implicit request: Request[JsValue]): Future[Result] =
    f((request.body \ fieldName).asOpt[Long])

  protected def fieldSeqStringOpt(fieldName: String)(f: Seq[String] => Future[Result])(implicit request: Request[JsValue]): Future[Result] =
    f((request.body \ fieldName).asOpt[Seq[String]].getOrElse(Seq.empty))

  protected def fieldSeqStringOptOpt(fieldName: String)(f: Option[Seq[String]] => Future[Result])(implicit request: Request[JsValue]): Future[Result] =
    f((request.body \ fieldName).asOpt[Seq[String]])

  protected def fieldBooleanOpt(fieldName: String)(f: Option[Boolean] => Future[Result])(implicit request: Request[JsValue]): Future[Result] =
    f((request.body \ fieldName).asOpt[Boolean])

  protected def fieldBooleanPrep(fieldName: String)(f: Boolean => Future[Result])(implicit request: Request[JsValue]): Future[Result] =
    f((request.body \ fieldName).asOpt[Boolean].getOrElse(false))

  protected def fieldString(fieldName: String)(f: String => Future[Result])(implicit request: Request[JsValue]): Future[Result] =
    (request.body \ fieldName).asOpt[String].fold(future(BadRequest("Not found field \"" + fieldName + "\"")))(f)

  protected def fieldInt(fieldName: String)(f: Int => Future[Result])(implicit request: Request[JsValue]): Future[Result] =
    (request.body \ fieldName).asOpt[Int].fold(future(BadRequest("Not found field \"" + fieldName + "\"")))(f)

  protected def fieldLong(fieldName: String)(f: Long => Future[Result])(implicit request: Request[JsValue]): Future[Result] =
    (request.body \ fieldName).asOpt[Long].fold(future(BadRequest("Not found field \"" + fieldName + "\"")))(f)

}