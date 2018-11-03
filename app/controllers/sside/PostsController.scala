package controllers.sside

import scala.concurrent.ExecutionContext

import com.typesafe.config.Config

import controllers.Authorizable
import javax.inject.Inject
import javax.inject.Singleton
import models.PostsFilter
import models.daos.DAO
import play.api.mvc.ControllerComponents
import controllers.AppContext

@Singleton
class PostsController @Inject() (cc: ControllerComponents, dao: DAO, config: Config)(implicit ec: ExecutionContext)
  extends Authorizable(cc, dao, config) {

  import scala.concurrent.Future.{ successful => future }

  def posts(pageId: Long) = Action.async { implicit request =>
    implicit val ac = new AppContext()
    optionalAuthorized { accountOpt =>
      dao.findPostsWithAccountsByCategoryTagIds(None, None, pageId, None, true) map { posts =>
        Ok(views.html.app.posts(posts, Some(PostsFilter.NEW)))
      }
    }
  }

  def postsByFilter(pageId: Long, filter: String) = Action.async { implicit request =>
    implicit val ac = new AppContext()
    optionalAuthorized { accountOpt =>
      dao.findPostsWithAccountsByCategoryTagIds(None, Some(filter), pageId, None, true) map { posts =>
        Ok(views.html.app.posts(posts, Some(filter)))
      }
    }
  }

  def viewPost(postId: Long) = Action.async { implicit request =>
    implicit val ac = new AppContext()
    optionalAuthorized { accountOpt =>
      dao.findPostWithAccountByPostId(postId) flatMap (
        _.fold(future(NotFound(""))) { post =>
          dao.findReviewsWithAccountsByCategoryTagIds(post.id, 1) map { reviews =>
            Ok(views.html.app.viewPost(post, reviews))
          }
        })
    }
  }

}

