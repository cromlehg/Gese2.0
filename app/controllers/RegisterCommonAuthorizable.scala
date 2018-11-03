package controllers

import java.io.IOException

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

import com.typesafe.config.Config

import javax.inject.Inject
import javax.inject.Singleton
import models.daos.DAO
import play.Logger
import play.api.mvc.ControllerComponents
import play.api.mvc.Result
import java.util.regex.Pattern
import services.Mailer
import services.MailerResponse

class RegisterCommonAuthorizable @Inject() (mailer: Mailer, cc: ControllerComponents, dao: DAO, config: Config)(implicit ec: ExecutionContext)
  extends Authorizable(cc, dao, config) {

  protected def createAccount(
    emailPatternName: String,
    accountType:      Int,
    login:            String,
    email:            String,
    companyNameOpt:   Option[String],
    ITNOpt:           Option[String],
    IECOpt:           Option[String])(
    f: models.Account => Result): Future[Result] = {
    dao.createAccount(
      login,
      email,
      AppConstants.INITIAL_DP,
      accountType,
      companyNameOpt,
      ITNOpt,
      IECOpt) map (_.fold(BadRequest("Can't create account")) { account =>

        try {
          mailer.sendVerificationToken(emailPatternName, account.email, account.login, account.confirmCode.get) match {
            case MailerResponse(true, _, _) =>
              f(account)
            case MailerResponse(false, status, msg) =>
              Logger.error("can't send email")
              Logger.error("status code: " + status)
              Logger.error("message: " + msg)
              BadRequest("Some problems")
          }
  
        } catch {
          case e: IOException =>
            Logger.error(e.toString())
            BadRequest("Seom problems")
        }

      })
  }

}

