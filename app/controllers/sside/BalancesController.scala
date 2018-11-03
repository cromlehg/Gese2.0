package controllers.sside

import scala.concurrent.ExecutionContext

import com.typesafe.config.Config

import controllers.Authorizable
import javax.inject.Inject
import javax.inject.Singleton
import models.daos.DAO
import play.api.mvc.ControllerComponents
import controllers.AppContext

@Singleton
class BalancesController @Inject() (cc: ControllerComponents, dao: DAO, config: Config)(implicit ec: ExecutionContext)
  extends Authorizable(cc, dao, config) {

  def balances(pageId: Long) = Action.async { implicit request =>
    implicit val ac = new AppContext()
    dao.findBalances(pageId) map { balances =>
      Ok(views.html.dev.balances(balances))
    }
  }

}

