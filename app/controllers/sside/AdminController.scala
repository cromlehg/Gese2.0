package controllers.sside

import scala.concurrent.ExecutionContext

import com.typesafe.config.Config

import controllers.Authorizable
import controllers.RegisterCommonAuthorizable
import controllers.AppContext
import javax.inject.Inject
import javax.inject.Singleton
import models.daos.DAO
import play.api.mvc.ControllerComponents
import scala.util.Random

import play.api.data.Forms.email
import play.api.data.Forms.text
import play.api.data.Forms.boolean
import play.api.data.Forms.mapping
import play.api.data.Forms.nonEmptyText
import play.api.data.Forms.number
import play.api.data.Forms.longNumber
import play.api.mvc.Flash

import play.api.data.Form
import models.AccountType

import java.io.IOException

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

import org.mindrot.jbcrypt.BCrypt

import com.typesafe.config.Config

import javax.inject.Inject
import javax.inject.Singleton
import models.AccountStatus
import models.AccountType
import models.CommentsViewType
import models.CurrencyType
import models.ErrCodes
import models.PostType
import models.RewardType
import models.TargetType
import models.daos.DAO
import play.Logger
import play.api.libs.json.JsArray
import play.api.libs.json.JsValue
import play.api.libs.json.Json
import play.api.mvc.ControllerComponents
import play.api.mvc.Request
import play.api.mvc.Result
import play.twirl.api.Html
import play.api.mvc.Action
import controllers.AppConstants
import models.ShortOptions
import controllers.JSONSupport

@Singleton
class AdminController @Inject() (cc: ControllerComponents, dao: DAO, config: Config)(implicit ec: ExecutionContext)
  extends Authorizable(cc, dao, config) with JSONSupport {

  import scala.concurrent.Future.{ successful => future }

  case class AuthData(val email: String, val pass: String)

  val authForm = Form(
    mapping(
      "email" -> nonEmptyText(3, 50),
      "pass"  -> nonEmptyText(8, 80))(AuthData.apply)(AuthData.unapply))

  case class ChargeData(val login: String, val currencyId: Int, val value: Long)

  val chargeForm = Form(
    mapping(
      "login"      -> nonEmptyText(3, 50),
      "currencyId" -> number,
      "value"      -> longNumber)(ChargeData.apply)(ChargeData.unapply))

  protected def onlyAdmin[T](f: models.Account => Future[Result])(implicit request: Request[T], ac: AppContext): Future[Result] =
    super.onlyAdmin(future(Redirect(controllers.sside.routes.AdminController.adminLogin())))(f)

  def admin() = Action.async { implicit request =>
    implicit val ac = new AppContext()
    onlyAdmin(account => future(Redirect(controllers.sside.routes.AdminController.adminOptions())))
  }

  def removePost(postId: Long) = Action.async { implicit request =>
    implicit val ac = new AppContext()
    onlyAdmin { account =>
      dao.findPostById(postId) flatMap {
        _.fold(future(BadRequest("Post with id " + postId + " not exists"))) { post =>
          dao.removePost(postId) map { isRemoved =>
            if (isRemoved)
              request.headers.get("referer")
                .fold(Redirect(controllers.sside.routes.AdminController.admin)) { url => Redirect(url) }
            else
              BadRequest("Couldn't remove post with id " + postId)
          }
        }
      }
    }
  }

  def charge = Action.async { implicit request =>
    implicit val ac = new AppContext()
    onlyAdmin(a => future(Ok(views.html.app.admin.charge(chargeForm))))
  }

  def chargeProcess = Action.async { implicit request =>
    implicit val ac = new AppContext()
    onlyAdmin { a =>
      chargeForm.bindFromRequest.fold(formWithErrors => future(BadRequest(views.html.app.admin.charge(formWithErrors))), { chargeData =>
        dao.findAccountByLoginOrEmail(chargeData.login) flatMap ( _.fold {
          val formWithErrors = chargeForm.fill(chargeData)
          future(BadRequest(views.html.app.admin.charge(formWithErrors)(Flash(formWithErrors.data) + ("error" -> "User not found!"), implicitly, implicitly)))
        } { u =>
          if(chargeData.value == 0) {
            val formWithErrors = chargeForm.fill(chargeData)
            future(BadRequest(views.html.app.admin.charge(formWithErrors)(Flash(formWithErrors.data) + ("error" -> "Charge value can't be empty or null!"), implicitly, implicitly)))
          } else
            dao.getCurrentAccountBalanceValue(u.id, chargeData.currencyId) flatMap { 
              _.fold { 
                val formWithErrors = chargeForm.fill(chargeData)
                future(BadRequest(views.html.app.admin.charge(formWithErrors)(Flash(formWithErrors.data) + ("error" -> ("Currecy with id " + chargeData.currencyId + " not exists!")), implicitly, implicitly)))
              } { value =>
                dao.chargeBalance(u.id, chargeData.currencyId, chargeData.value) map { 
                  _.fold {
                    val formWithErrors = chargeForm.fill(chargeData)
                    BadRequest(views.html.app.admin.charge(formWithErrors)(Flash(formWithErrors.data) + ("error" -> "Error occurs during balance changing, you can try later!"), implicitly, implicitly))
                  } { newBalance =>
                    Redirect(controllers.sside.routes.AdminController.charge)
                      .flashing("success" -> ("Balances for @" + u.login +
                         " updated with currency id " + chargeData.currencyId +
                         " from " + value + " to " + newBalance))
                  }
                }
              }
            }
        })
      })
    }
  }


  def moderatePost(postId: Long, moderateStatus: Int) = Action.async { implicit request =>
    models.ModerateStatus.strById(moderateStatus).fold(future(BadRequest("Wrong moderate status id " + moderateStatus))) { _ =>
      implicit val ac = new AppContext()
      onlyAdmin(account =>
        dao.setModerateStatusToPost(postId, moderateStatus) map { success =>
          if (success)
            request.headers.get("referer")
              .fold(Redirect(controllers.sside.routes.AdminController.admin)) { url => Redirect(url) }
          else
            BadRequest("Couldn't set moderate status " + moderateStatus + " for post " + postId)
        })
    }
  }

  def setStatus(accountId: Long, status: Int) = Action.async { implicit request =>
    models.UserStatus.strById(status).fold(future(BadRequest("Wrong status id " + status))) { _ =>
      implicit val ac = new AppContext()
      onlyAdmin(account =>
        dao.setAccountStatus(accountId, status) map { success =>
          if (success)
            request.headers.get("referer")
              .fold(Redirect(controllers.sside.routes.AdminController.admin)) { url => Redirect(url) }
          else
            BadRequest("Couldn't set status " + status + " for account " + accountId)
        })
    }
  }

  def adminAccounts(pageId: Int, filterOpt: Option[String]) = Action.async { implicit request =>
    if(filterOpt.isDefined && !filterOpt.get.matches("[a-z0-9]{1,}")) {
      future(request.headers.get("referer")
        .fold { 
          Redirect(controllers.sside.routes.AdminController.adminAccounts(1, None))
            .flashing("error" -> "Search string must contains only a-b or 0-9 symbols!")
        } { url => 
          Redirect(url)
            .flashing("error" -> "Search string must contains only a-b or 0-9 symbols!")
        })
    } else {
      implicit val ac = new AppContext()
      onlyAdmin(a =>
        dao.getAccountsPagesCount(filterOpt) flatMap { pagesCount =>
          if (pageId > pagesCount) future(BadRequest("Page not found " + pageId)) else
            dao.getAccounts(filterOpt, pageId) map { accounts =>
              Ok(views.html.app.admin.adminAccounts(
                  a, 
                  accounts, 
                  config getString "gese.admin.view.account", 
                  config getString "gese.admin.view.accountby", 
                  pageId, 
                  pagesCount,
                  filterOpt))
            }
        })
    }
  }

  def adminPosts(pageId: Int) = Action.async { implicit request =>
    implicit val ac = new AppContext()
    onlyAdmin(a =>
      dao.getAdminPostsPagesCount() flatMap { pagesCount =>
        if (pageId > pagesCount)
          future(Ok(views.html.app.admin.adminPosts(a, Seq(), config getString "gese.admin.view.post", pageId, pagesCount)))
        else
          dao.getAdminPosts(pageId) map { posts =>
            Ok(views.html.app.admin.adminPosts(a, posts, config getString "gese.admin.view.post", pageId, pagesCount))
          }
      })
  }

  def adminModeratePosts(pageId: Int) = Action.async { implicit request =>
    implicit val ac = new AppContext()
    onlyAdmin(a =>
      dao.getAdminModeratePostsPagesCount() flatMap { pagesCount =>
        if (pageId > pagesCount)
          future(Ok(views.html.app.admin.adminModeratePosts(a, Seq(), config getString "gese.admin.view.post", pageId, pagesCount)))
        else
          dao.getAdminModeratePosts(pageId) map { posts =>
            Ok(views.html.app.admin.adminModeratePosts(a, posts, config getString "gese.admin.view.post", pageId, pagesCount))
          }
      })
  }

  def adminStats = Action.async { implicit request =>
    implicit val ac = new AppContext()
    onlyAdmin(a => dao.getStats map { stats =>
      Ok(views.html.app.admin.adminStats(stats))
    })
  }

  def adminLogout = Action.async { implicit request =>
    implicit val ac = new AppContext()
    super.logout(Redirect(controllers.sside.routes.AdminController.adminLogin()))
  }

  def adminLogin() = Action.async { implicit request =>
    implicit val ac = new AppContext()
    notAuthorized(future(Ok(views.html.app.admin.adminLogin(authForm))))
  }

  def adminLoginProcess() = Action.async { implicit request =>
    implicit val ac = new AppContext()
    notAuthorized {
      authForm.bindFromRequest.fold(formWithErrors => future(BadRequest(views.html.app.admin.adminLogin(formWithErrors))), { authData =>
        authCheckBlock(authData.email, authData.pass, models.Roles.ADMIN) { msg =>
          val formWithErrors = authForm.fill(authData)
          future(BadRequest(views.html.app.admin.adminLogin(formWithErrors)(Flash(formWithErrors.data) + ("error" -> msg), implicitly, implicitly)))
        } {
          case (account, session) =>
            future(Redirect(controllers.sside.routes.AdminController.adminOptions()))
        }
      })
    }
  }

  def adminOptions() = Action.async { implicit request =>
    implicit val ac = new AppContext()
    onlyAdmin(a => dao.getShortOptions map (t => Ok(views.html.app.admin.adminOptions(a, t))))
  }

  def switchBooleanOption() = Action.async(parse.json) { implicit request =>
    implicit val ac = new AppContext()
    onlyAdmin(a => fieldString("name")(name => dao.getShortOptionByName(name) flatMap (_.fold(future(BadRequest("Not found"))) { option =>
      if (option.ttype != ShortOptions.TYPE_BOOLEAN) future(BadRequest("Option must be boolean to switch")) else
        dao.updateShortOptionByName(name, if (option.toBoolean) "false" else "true") map {
          _.fold(BadRequest("Can't update option"))(t => Ok(t.toBoolean.toString))
        }
    })))
  }

}

