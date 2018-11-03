package controllers

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

import org.mindrot.jbcrypt.BCrypt

import com.typesafe.config.Config

import javax.inject.Inject
import models.daos.DAO
import play.api.i18n.I18nSupport
import play.api.mvc.AbstractController
import play.api.mvc.ControllerComponents
import play.api.mvc.Request
import play.api.mvc.Result
import play.api.mvc.Action
import models.AccountStatus
import models.Roles
import play.Logger

class Authorizable @Inject() (cc: ControllerComponents, dao: DAO, config: Config)(implicit ec: ExecutionContext)
  extends AbstractController(cc)
  with I18nSupport {

  import scala.concurrent.Future.{ successful => future }

  val SESSION_KEY = "session_key"

  protected def notAuthorized[T](f: Future[Result])(implicit request: Request[T], ac: AppContext): Future[Result] =
    request.session.get(SESSION_KEY).fold(f)(curSessionKey =>
      future(BadRequest("You should logout before")))

  protected def sessionNotExpired(account: models.Account)(f: models.Account => Future[Result])(implicit ac: AppContext): Future[Result] =
    account.sessionOpt.fold(Future.successful(BadRequest("Empty user session"))) { session =>
      ac.authorizedOpt = Some(account)
      f(account)
    }

  protected def sessionNotExpired2(account: models.Account)(notAuthorized: Future[Result])(f: models.Account => Future[Result])(implicit ac: AppContext): Future[Result] =
    account.sessionOpt.fold(notAuthorized) { session =>
      ac.authorizedOpt = Some(account)
      f(account)
    }

  protected def logout[T](f: Result)(implicit request: Request[T], ac: AppContext): Future[Result] =
    request.session.get(SESSION_KEY).fold(future(BadRequest("You shuld authorize before")))(curSessionKey =>
      dao.invalidateSessionBySessionKeyAndIP(curSessionKey, request.remoteAddress) map (t => f.withNewSession))

  // access only if user authorized ???
  protected def onlyAuthorizedOwnerUserOrSelf[T](userId: Option[Long])(f: models.Account => Future[Result])(implicit request: Request[T], ac: AppContext): Future[Result] =
    userId.fold(onlyAuthorized(f))(userId => onlyAuthorizedOwnerUser(userId)(f))

  protected def optionalAuthorized[T](f: Option[models.Account] => Future[Result])(implicit request: Request[T], ac: AppContext): Future[Result] =
    request.session.get(SESSION_KEY).fold(f(None))(curSessionKey =>
      dao.findAccountBySessionKeyAndIPWithBalancesAndRoles(curSessionKey, request.remoteAddress)
        flatMap (_.fold(f(None))(user => user.sessionOpt.fold(f(None)) { session =>
          ac.authorizedOpt = Some(user)
          f(Some(user))
        })))

  protected def onlyAdmin[T](notAuthorized: Future[Result])(f: models.Account => Future[Result])(implicit request: Request[T], ac: AppContext): Future[Result] =
    onlyAuthorized(notAuthorized)(a => if (a.roles contains Roles.ADMIN) f(a) else notAuthorized)

  protected def authorizedNotLocked[T](notAuthorized: Future[Result])(f: models.Account => Future[Result])(implicit request: Request[T], ac: AppContext): Future[Result] =
    onlyAuthorized(notAuthorized)(a => if (a.userStatus == models.UserStatus.NORMAL) f(a) else notAuthorized)

  protected def authorizedNotLocked[T](f: models.Account => Future[Result])(implicit request: Request[T], ac: AppContext): Future[Result] =
    onlyAuthorized(a => if (a.userStatus == models.UserStatus.NORMAL) f(a) else future(BadRequest("Your account is blocked!")))

  protected def onlyAuthorized[T](notAuthorized: Future[Result])(f: models.Account => Future[Result])(implicit request: Request[T], ac: AppContext): Future[Result] =
    request.session.get(SESSION_KEY).fold(notAuthorized)(curSessionKey =>
      dao.findAccountBySessionKeyAndIPWithBalancesAndRoles(curSessionKey, request.remoteAddress)
        flatMap (_.fold(notAuthorized)(user =>
          sessionNotExpired2(user)(notAuthorized)(f))))

  protected def onlyAuthorized[T](f: models.Account => Future[Result])(implicit request: Request[T], ac: AppContext): Future[Result] =
    request.session.get(SESSION_KEY).fold(future(BadRequest("Session not found. You shoud authorize before")))(curSessionKey =>
      dao.findAccountBySessionKeyAndIPWithBalancesAndRoles(curSessionKey, request.remoteAddress)
        flatMap (_.fold(future(BadRequest("Can't find session. You should authorize before")))(user =>
          sessionNotExpired(user)(f))))

  protected def onlyAuthorizedOwnerUser[T](userId: Long)(f: models.Account => Future[Result])(implicit request: Request[T], ac: AppContext): Future[Result] =
    request.session.get(SESSION_KEY).fold(future(BadRequest("Session not found. You shoud authorize before")))(curSessionKey =>
      dao.findAccountBySessionKeyAndIPWithBalancesAndRoles(curSessionKey, request.remoteAddress)
        flatMap (_.fold(future(BadRequest("Can't find session. You should authorize before")))(user =>
          sessionNotExpired(user)(if (user.id == userId) f else { account => future(BadRequest("Access forbidden")) }))))

  protected def sessionKeyStr(userId: Long)(implicit request: Request[_]) =
    BCrypt.hashpw(userId + System.currentTimeMillis + request.remoteAddress, BCrypt.gensalt())

  protected def authCheckBlock(loginOrEmail: String, pwd: String, role: Int = models.Roles.CLIENT)(error: String => Future[Result])(success: (models.Account, models.Session) => Future[Result])(implicit request: Request[_]): Future[Result] =
    dao.findAccountByLoginOrEmail(loginOrEmail) flatMap (_.fold(error("Login or password not found")) { account =>
      if (account.roles contains role) {
        if (account.accountStatus == AccountStatus.WAITE_CONFIRMATION) error("Email wait confirmation!") else
          account.hash.fold(error("Login or password not found")) { hash =>
            if (BCrypt.checkpw(pwd, hash)) {

              def createSession = {
                val expireTime = System.currentTimeMillis + AppConstants.SESSION_EXPIRE_TYME
                val sessionKey = sessionKeyStr(account.id)
                dao.createSession(
                  account.id,
                  request.remoteAddress,
                  sessionKey,
                  System.currentTimeMillis,
                  expireTime) flatMap (_.fold(future(BadRequest("Coludn't create session"))) { session =>
                    success(account, session).map(_.withSession(SESSION_KEY -> sessionKey))
                  })
              }

              request.session.get(SESSION_KEY).fold(createSession)(curSessionKey =>
                dao.findSessionByAccountIdSessionKeyAndIP(account.id, request.remoteAddress, curSessionKey)
                  flatMap (_.fold(createSession)(session => error("You should logout before."))))

            } else error("Login or password not found")

          }
      } else error("Account not have this role")
    })

  //  def withAppContext[T](bodyParser: play.api.mvc.BodyParser[T])(f: (Request[T], AppContext) => Future[Result]): Action[T] =
  //    Action.async[T](bodyParser)(request => f(request, new AppContext()))
  //
  //  def withAppContext(f: (Request[play.api.mvc.AnyContent], AppContext) => Future[Result]): Action[play.api.mvc.AnyContent] =
  //    withAppContext(parse.anyContent)(f)

}
