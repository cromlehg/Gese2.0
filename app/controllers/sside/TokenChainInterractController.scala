package controllers.sside

import java.io.IOException

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

import org.mindrot.jbcrypt.BCrypt

import com.typesafe.config.Config

import javax.inject.Inject
import javax.inject.Singleton
import models.AccountStatus
import models.CommentsViewType
import models.CurrencyType
import models.ErrCodes
import models.PostType
import models.RewardType
import models.TargetType
import models.daos.DAO
import play.Logger
import play.api.i18n.I18nSupport
import play.api.libs.json.JsArray
import play.api.libs.json.JsObject
import play.api.libs.json.JsValue
import play.api.libs.json.Json
import play.api.mvc.AbstractController
import play.api.mvc.ControllerComponents
import play.api.mvc.Request
import play.api.mvc.Result
import models.AccountType
import controllers.Authorizable

@Singleton
class TokenChainInterractController @Inject() (cc: ControllerComponents, dao: DAO, config: Config)(implicit ec: ExecutionContext)
  extends Authorizable(cc, dao, config) {

  import scala.concurrent.Future.{ successful => future }


}

