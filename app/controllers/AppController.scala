package controllers

import scala.concurrent.ExecutionContext

import javax.inject.Inject
import javax.inject.Singleton
import models.daos.DAO
import play.api.i18n.I18nSupport
import play.api.mvc.AbstractController
import play.api.mvc.AnyContent
import play.api.mvc.ControllerComponents
import play.api.mvc.Request
import com.typesafe.config.Config

/**
 * This controller creates an `Action` to handle HTTP requests to the
 * application's home page.
 */
@Singleton
class AppController @Inject() (cc: ControllerComponents, dao: DAO, config: Config)(implicit ec: ExecutionContext)
  extends AbstractController(cc)
  with I18nSupport {

  /**
   * Create an Action to render an HTML page.
   *
   * The configuration in the `routes` file means that this method
   * will be called when the application receives a `GET` request with
   * a path of `/`.
   */

  def index = Action(Redirect(controllers.sside.routes.AdminController.adminOptions()))

  //  def index() = Action { implicit request: Request[AnyContent] =>
  //    Ok(views.html.status())
  //  }

}
