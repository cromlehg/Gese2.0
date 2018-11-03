package controllers

import java.io.IOException

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

import org.mindrot.jbcrypt.BCrypt

import services.Mailer
import services.MailerResponse
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
import models.AccountLevel
import play.api.libs.json._
import play.api.libs.functional.syntax._
import models.Geo
import models.ShortOptions

@Singleton
class APIv1 @Inject() (mailer: Mailer, cc: ControllerComponents, dao: DAO, config: Config)(implicit ec: ExecutionContext)
  extends RegisterCommonAuthorizable(mailer, cc, dao, config) with JSONSupport {

  import scala.concurrent.Future.{ successful => future }

  val NAME_GET_TXS = "getTransactions"

  val NAME_GET_TXS_PAGES = "getTransactionsPages"

  val NAME_GET_POSTS = "getPosts"

  val NAME_GET_USER = "getUser"

  val NAME_GET_USER_INFO = "getUserInfo"

  val NAME_SIGN_IN = "signIn"

  val NAME_REGISTER = "register"

  val NAME_LOGOUT = "logout"

  val NAME_UPDATE_USER_AVATAR = "updateUserAvatar"

  val NAME_UPDATE_USER_SEX = "updateUserSex"

  val NAME_UPDATE_USER_BIRTHDAY = "updateUserBirthday"

  val NAME_UPDATE_USER_EDUCATION = "updateUserEducation"

  val NAME_UPDATE_USER_ABOUT = "updateUserAbout"

  val NAME_UPDATE_USER_BG = "updateUserBackground"

  val NAME_UPDATE_USER_NAME = "updateUserName"

  val NAME_UPDATE_USER_SURNAME = "updateUserSurname"

  val NAME_UPDATE_COMPANY_ITN = "updateCompanyITN"

  val NAME_UPDATE_COMPANY_IEC = "updateCompanyIEC"

  val NAME_CREATE_POST = "createPost"

  val NAME_PROMOTE = "promote"

  val NAME_GET_COMMENTS = "getComments"

  val NAME_GET_TAGS = "getTags"

  val NAME_VERIFY_EMAIL = "verifyEmail"

  val NAME_POST_COMMENT = "postComment"

  val NAME_IS_LOGIN_EXISTS = "isLoginExists"

  val NAME_IS_EMAIL_EXISTS = "isEmailExists"

  val NAME_TRANSFER = "transfer"

  val NAME_GET_POST = "getPost"

  val NAME_LIKE = "like"

  val NAME_SEARCH = "search"

  val NAME_EXCHANGE = "exchange"

  val NAME_APPROVE_TX = "approveTransaction"

  val NAME_TEST = "test"

  val NAME_NEEDS_VERIFY = "needsVerify"

  val NAME_DISABLE_SHOW_BRAND = "disableShowBrand"

  val NAME_CREATE_PRODUCT = "createProduct"

  val NAME_GET_PRODUCTS = "getProducts"

  val NAME_GET_PRODUCT = "getProduct"

  val NAME_CREATE_CAMPAING = "createCampaign"

  val NAME_GET_CAMPAINGS = "getCampaigns"

  val NAME_CREATE_BATCH = "createBatch"

  val NAME_GET_BATCHES = "getBatches"

  val NAME_GET_REVIEWS = "getReviews"

  val NAME_GET_BATCH_CODES = "getBatchCodes"

  val NAME_FIND_PRODUCTS_BY_NAME = "findProductsByName"

  val NAME_CHECK_BOTTLE = "checkBottle"

  val NAME_GET_BOTTLE_INFO = "getBottleInfo"

  val NAME_CHECK_REWARD = "checkReward"

  val NAME_PROCESS_REWARD = "processReward"

  val NAME_CALCULATE_BUDGET = "calculateBudget"

  val NAME_SET_COMMENT_STATUS = "setCommentStatus"

  val NAME_GET_PRODUCTS_PAGES = "getProductsPages"

  val NAME_GET_REVIEWS_PAGES = "getReviewsPages"

  val NAME_GET_TAGS_PAGES = "getTagsPages"

  val NAME_GET_COMMENTS_PAGES = "getCommentsPages"

  val NAME_GET_POSTS_PAGES = "getPostsPages"

  val NAME_IS_OPTION_TRUE = "isOptionTrue"

  def api = Action.async(parse.json) { implicit request =>
    implicit val sc = new AppContext()
    fieldString("name") { name =>
      name match {
        case NAME_IS_OPTION_TRUE => isOptionTrue()
        case NAME_GET_POSTS => getPosts()
        case NAME_SIGN_IN => signIn()
        case NAME_REGISTER => register()
        case NAME_LOGOUT => logout()
        case NAME_UPDATE_USER_AVATAR => updateUserAvatar()
        case NAME_GET_USER => getUser()
        case NAME_CREATE_POST => createPost()
        case NAME_GET_TXS => getTransactions()
        case NAME_UPDATE_USER_NAME => updateUserName()
        case NAME_UPDATE_USER_SURNAME => updateUserSurname()
        case NAME_UPDATE_USER_BG => updateUserBackground()
        case NAME_GET_COMMENTS => getComments()
        case NAME_IS_LOGIN_EXISTS => isLoginExists()
        case NAME_IS_EMAIL_EXISTS => isEmailExists()
        case NAME_GET_USER_INFO => getUserInfo()
        case NAME_POST_COMMENT => postComment()
        case NAME_GET_POST => getPost()
        case NAME_GET_PRODUCT => getProduct()
        case NAME_LIKE => like()
        case NAME_SEARCH => search()
        case NAME_TRANSFER => transfer()
        case NAME_PROMOTE => promote()
        case NAME_EXCHANGE => exchange()
        case NAME_GET_TAGS => getTags()
        case NAME_GET_TAGS_PAGES => getTagsPages()
        case NAME_VERIFY_EMAIL => verifyEmail()
        case NAME_APPROVE_TX => approveTransaction()
        case NAME_UPDATE_USER_ABOUT => updateUserAbout()
        case NAME_UPDATE_USER_SEX => updateUserSex()
        case NAME_UPDATE_USER_BIRTHDAY => updateUserBirthday()
        case NAME_UPDATE_USER_EDUCATION => updateUserEducation()
        case NAME_NEEDS_VERIFY => needsVerify()
        case NAME_DISABLE_SHOW_BRAND => disableShowBrand()
        case NAME_UPDATE_COMPANY_ITN => updateCompanyITN()
        case NAME_UPDATE_COMPANY_IEC => updateCompanyIEC()
        case NAME_CREATE_PRODUCT => createProduct()
        case NAME_GET_PRODUCTS => getProducts()
        case NAME_GET_PRODUCTS_PAGES => getProductsPages()
        case NAME_GET_REVIEWS_PAGES => getReviewsPages()
        case NAME_CREATE_CAMPAING => createCampaing()
        case NAME_GET_CAMPAINGS => getCampaings()
        case NAME_CREATE_BATCH => createBatch()
        case NAME_GET_BATCHES => getBatches()
        case NAME_GET_BATCH_CODES => getBatchCodes()
        case NAME_FIND_PRODUCTS_BY_NAME => findProductsByName()
        case NAME_GET_REVIEWS => getReviews()
        case NAME_CHECK_BOTTLE => checkBottle()
        case NAME_GET_BOTTLE_INFO => getBottleInfo()
        case NAME_CHECK_REWARD => checkReward()
        case NAME_PROCESS_REWARD => processReward()
        case NAME_CALCULATE_BUDGET => calculateBudget()
        case NAME_SET_COMMENT_STATUS => setCommentStatus()
        case NAME_GET_COMMENTS_PAGES => getCommentsPages()
        case NAME_GET_TXS_PAGES => getTransactionsPages()
        case NAME_GET_POSTS_PAGES => getPostsPages()
        case _ => future(BadRequest("Function " + name + " not found"))
      }
    }
  }

  private def withNameOrId(
    idFieldName: String,
    nameFieldName: String,
    idByName: String => Future[Option[Long]],
    f: Long => Future[Result])(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    fieldLongOpt(idFieldName)(_.fold(
      fieldString(nameFieldName) { str =>
        val preapred = str.trim()
        if (preapred.length < 1) future(BadRequest("Should be 1 symbol at least")) else
          idByName(preapred) flatMap (_.fold(future(BadRequest("Not found")))(f))
      })(f))

  private def withNameOrIdOpt(
    idFieldName: String,
    nameFieldName: String,
    idByName: String => Future[Option[Long]])(
    f1: Long => Future[Result])(
    f2: Future[Result])(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    fieldLongOpt(idFieldName)(_.fold(fieldStringOpt(nameFieldName)(_.fold(f2) { str =>
      val preapred = str.trim()
      if (preapred.length < 1) future(BadRequest("Should be 1 symbol at least")) else
        idByName(preapred) flatMap (_.fold(future(BadRequest("Not found")))(f1))
    }))(f1))

  private def withNameOrIdSingleOpt(
    idFieldName: String,
    nameFieldName: String,
    idByName: String => Future[Option[Long]])(
    f: Option[Long] => Future[Result])(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    withNameOrIdOpt(idFieldName, nameFieldName, idByName)(t => f(Some(t)))(f(None))

  private def withAccountNameOrIdOpt(f1: Long => Future[Result])(f2: Future[Result])(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    withNameOrIdOpt("account_id", "login", dao.findAccountIdByLoginOrEmail)(f1)(f2)

  private def withAccountNameOrIdSingleOpt(f: Option[Long] => Future[Result])(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    withNameOrIdSingleOpt("account_id", "login", dao.findAccountIdByLoginOrEmail)(f)

  private def withProductNameOrId(f: Long => Future[Result])(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    withNameOrId("product_id", "product_name", dao.findProductIdByName, f)

  private def withAccountNameOrId(f: Long => Future[Result])(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    withNameOrId("account_id", "login", dao.findAccountIdByLoginOrEmail, f)

  private def getCampaings()(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    fieldLongOpt("product_id")(productIdOpt => fieldBooleanPrep("current")(isCurrent => fieldLongOpt("page_id")(pageIdOpt => withAccountNameOrIdSingleOpt { idOpt =>
      dao.getCampaings(idOpt, productIdOpt, isCurrent, pageIdOpt.getOrElse(1L)) map { campaigns => Ok(jsonResultOk(JsArray(campaigns.map(_.toJson)))) }
    })))

  private def getBatches()(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    fieldLongOpt("product_id")(productIdOpt => fieldLongOpt("page_id")(pageIdOpt => withAccountNameOrId { id =>
      dao.getBatchesWithBalances(id, productIdOpt, pageIdOpt.getOrElse(1L)) map { batches => Ok(jsonResultOk(JsArray(batches.map(_.toJson)))) }
    }))

  private def enoughFunds(f: (Long, Int, Long, models.Account) => Future[Result])(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    calculateBudget { (price, count, balance, account) =>
      dao.enoughFunds(account.id, CurrencyType.DOLLAR, balance) flatMap { enough =>
        if (enough)
          f(price, count, balance, account)
        else
          future(BadRequest("You have no enough funds. You shhould have " + balance + " of dollars"))
      }
    }

  private def calculateBudget()(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    calculateBudget { (price, count, balance, account) => future(Ok(jsonResultOk(Json.toJson(balance)))) }

  private def calculateBudget(f: (Long, Int, Long, models.Account) => Future[Result])(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    fieldLong("price")(price => fieldInt("count") { count =>
      if (price > 0) {
        if (count < 1) future(BadRequest("Count should be positive non 0")) else
          authorizedNotLocked(account => f(price, count, (price * count * models.AccountLevel.MAX_K + 1).toLong, account))
      } else future(BadRequest("Price should be positive and non 0"))
    })

  private def onlyPostOwner(postId: Long, accountId: Long)(f: () => Future[Result])(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    dao.isPostOwner(postId, accountId) flatMap (isOwner => if (isOwner) f() else future(BadRequest("Account " + accountId + " are not owner of post " + postId + "")))

  private def createCampaing()(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    fieldStringOpt("descr")(descrOpt => fieldString("title")(title => fieldLong("start")(start => fieldLong("end")(end =>
      enoughFunds { (price, count, balance, account) =>
        withProductNameOrId { productId =>
          onlyPostOwner(productId, account.id) { () =>
            dao.createCampaing(productId, price, count, start, end, descrOpt, balance, title) map (
              _.fold(BadRequest("Some problems during campaign creation"))(t => Ok(jsonResultOk(t.toJson))))
          }
        }
      }))))

  private def processReward()(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    fieldString("code")(code => fieldInt("rate") { rate =>
      if (rate > 5 || rate < 0) future(BadRequest("Wrong rate " + rate + ". Rate should be from 0 to 5!")) else
        authorizedNotLocked { account =>
          dao.processRewardForItemByCode(account.id, code, rate) map (_.fold(BadRequest("Some problems during processing reward for code " + code)) { t =>
            Ok(jsonResultOk(t.toJson))
          })
        }
    })

  private def checkReward()(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    fieldString("code")(code => authorizedNotLocked { account =>
      dao.getNotBoughtItemWithBatchByCode(code) flatMap (_.fold(future(BadRequest("No free items with code " + code))) {
        case (item, batch) =>
          dao.getActiveCampaingsPricesForProduct(batch.productId) map { campaingsPrices =>
            Ok(jsonResultOk(Json.toJson(AccountLevel.kByRate(account.rate) * (campaingsPrices.sum + batch.price))))
          }
      })
    })

  private def createBatch()(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    fieldLong("product_id")(productId =>
      enoughFunds { (price, count, balance, account) =>
        existsProductInCompany(productId, account.id) {
          onlyPostOwner(productId, account.id) { () =>
            dao.createBatch(productId, price, count, balance) map (
              _.fold(BadRequest("Some problems during batch creation"))(t => Ok(jsonResultOk(t.toJson))))
          }
        }
      })

  private def checkBottle()(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    fieldString("code")(code =>
      optionalAuthorized { optAccount =>

        dao.isExistsBottle(code) flatMap { exists =>
          if (exists) {
            def optionalUpdateGEO =
              dao.checkItemAndUpdateGeo(code, (request.body \ "geo").asOpt[Geo]) map (
                _.fold(BadRequest("Some problems during bottle update"))(t => Ok(jsonResultOk(t.toJson))))

            optAccount.fold(optionalUpdateGEO) { account =>
              dao.scheduleNotification(code, account.id) flatMap (scheduled => optionalUpdateGEO)
            }
          } else future(BadRequest(jsonResultError(ErrCodes.EC_ITEM_NOT_EXISTS, ErrCodes.STR_ITEM_NOT_EXISTS)))
        }

      })

  private def setCommentStatus()(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    fieldLong("comment_id")(commentId => fieldString("status") { statusStr =>
      val statusOpt = models.CommentStatus.idByStr(statusStr)
      statusOpt.fold(future(BadRequest("Wrong status " + statusStr))) { status =>
        authorizedNotLocked(account =>
          dao.isAccountOwnerOfComment(account.id, commentId) flatMap { isOwner =>
            if (isOwner)
              dao.setCommentStatus(commentId, status) map { setted =>
                if (setted) Ok(jsonResultOk()) else BadRequest("Some problmes during set commet status " + commentId)
              }
            else
              future(BadRequest("You not authorized to change comment " + commentId))
          })
      }
    })

  private def getBottleInfo()(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    fieldString("code")(code =>
      dao.getBottleInfo(code) map (
        _.fold(BadRequest("Some problems during gettting bottle info"))(t => Ok(jsonResultOk(t.toJson)))))

  private def getBatchCodes()(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    fieldLong("batch_id")(batchId => authorizedNotLocked { account =>
      dao.isAccountOwnerOfBatch(account.id, batchId) flatMap { isOwner =>
        if (isOwner)
          dao.getBatchCodes(batchId) map { codes => Ok(jsonResultOk(Json.toJson(codes.map(t => (t._1.toString, t._2))))) }
        else
          future(BadRequest("You are not authorized to view this batch"))
      }
    })

  private def needsVerify()(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    fieldString("login")(login => fieldString("code") { code =>
      dao.findAccountByConfirmCodeAndLogin(login, code) map (_.fold(BadRequest("Confirm code not found")) { user =>
        Ok(jsonResultOk(user.toJson))
      })
    })

  private def findProductsByName()(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    fieldLongOpt("page_id")(pageIdOpt =>
      fieldString("product_name")(productName =>
        if (productName.trim.isEmpty) future(BadRequest("Search pattern can't be empty")) else
          withAccountNameOrIdSingleOpt { idOpt =>
            dao.getProductIdAndNames(idOpt, productName, pageIdOpt.getOrElse(1L))
              .map { values => Ok(jsonResultOk(JsArray(values.map(t => Json.obj("id" -> t._1, "name" -> t._2))))) }
          }))

  private def verifyEmail()(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    fieldString("login")(login => fieldString("code")(code => fieldString("pwd") { pwd =>
      val performedPwd = pwd.trim()
      if (performedPwd.size < AppConstants.PWD_MIN_LENGTH)
        future(BadRequest(jsonResultError(ErrCodes.EC_PASSWORD_MIN_LENGTH, ErrCodes.STR_PASSWORD_MIN_LENGTH)))
      else
        dao.findAccountByConfirmCodeAndLogin(login, code) flatMap (_.fold(future(BadRequest("Confirm code not found"))) { user =>
          dao.emailVerified(login, code, pwd) map (_.fold(BadRequest("Can't verify email")) { userVerified =>
            Ok(jsonResultOk(userVerified.toJson))
          })
        })
    }))

  private def signIn()(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    fieldString("login")(login => fieldString("pwd") { pwd =>
      dao.isLoginExists(login) flatMap { exists =>
        if (exists) {
          dao.findAccountByLoginOrEmailWithBalances(login).flatMap(_.fold(future(BadRequest("Login or password incorrect"))) { user =>
            if (user.accountStatus == AccountStatus.WAITE_CONFIRMATION)
              future(BadRequest(jsonResultError(ErrCodes.EC_EMAIL_NOT_VERIFIED, ErrCodes.STR_EMAIL_NOT_VERIFIED)))
            else if(user.userStatus == models.UserStatus.LOCKED)
              future(BadRequest(jsonResultError(ErrCodes.EC_ACCOUNT_LOCKED, ErrCodes.STR_ACCOUNT_LOCKED)))
            else
              user.hash.fold(future(BadRequest("Not have password yet"))) { hash =>
                if (BCrypt.checkpw(pwd, hash)) {

                  def createSession = {
                    val expireTime = System.currentTimeMillis + AppConstants.SESSION_EXPIRE_TYME
                    val sessionKey = sessionKeyStr(user.id)
                    dao.createSession(
                      user.id,
                      request.remoteAddress,
                      sessionKey,
                      System.currentTimeMillis,
                      expireTime) map (_.fold(BadRequest("Coludn't create session")) { session =>
                        ac.authorizedOpt = Some(user)
                        Ok(jsonResultOk(user.toJson)).withSession(SESSION_KEY -> sessionKey)
                      })
                  }

                  request.session.get(SESSION_KEY).fold(createSession)(curSessionKey =>
                    dao.findSessionByAccountIdSessionKeyAndIP(user.id, request.remoteAddress, curSessionKey)
                      flatMap (_.fold(createSession)(session =>
                        future(BadRequest("Session already created and active. You should invalidate session before.")))))
                } else future(BadRequest("Login or password incorrect"))
              }
          })
        } else future(BadRequest("User not found"))
      }
    })

  private def getPosts()(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    fieldLong("page_id")(pageId => fieldStringOpt("filter") { filterName =>
      optionalAuthorized { optUser =>
        shortInContext {
          fieldSeqStringOptOpt("tags") { tagNamesOpt: Option[Seq[String]] =>
            val tagNamesOptPrepared = tagNamesOpt.map(_.map(_.trim.toLowerCase))
            if (tagNamesOptPrepared.isDefined && tagNamesOptPrepared.get.length > AppConstants.TAGS_PER_POST_LIMIT) future(BadRequest("You have more than " + AppConstants.TAGS_PER_POST_LIMIT + " tags"))
            else if (tagNamesOptPrepared.isDefined && tagNamesOptPrepared.get.exists(_.length < AppConstants.TAG_SIZE_LIMIT)) future(BadRequest("Each tag length should be more than " + (AppConstants.TAG_SIZE_LIMIT - 1))) else {
              val userIdOpt = optUser.map(_.id)

              withAccountNameOrIdSingleOpt(idOpt =>
                dao.findPostsWithAccountsByCategoryTagNames(userIdOpt, idOpt, filterName, pageId, tagNamesOptPrepared, idOpt.isDefined) map { posts =>
                  Ok(jsonResultOk(JsArray(posts.map(_.toJson))))
                })
            }
          }
        }
      }
    })

  private def getPostsPages()(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    fieldStringOpt("filter") { filterName =>
      fieldSeqStringOptOpt("tags") { tagNamesOpt: Option[Seq[String]] =>
        val tagNamesOptPrepared = tagNamesOpt.map(_.map(_.trim.toLowerCase))
        if (tagNamesOptPrepared.isDefined && tagNamesOptPrepared.get.length > AppConstants.TAGS_PER_POST_LIMIT) future(BadRequest("You have more than " + AppConstants.TAGS_PER_POST_LIMIT + " tags"))
        else if (tagNamesOptPrepared.isDefined && tagNamesOptPrepared.get.exists(_.length < AppConstants.TAG_SIZE_LIMIT)) future(BadRequest("Each tag length should be more than " + (AppConstants.TAG_SIZE_LIMIT - 1))) else {
          withAccountNameOrIdSingleOpt(idOpt =>
            dao.findPostsPagesWithAccountsByCategoryTagNames(idOpt, filterName, tagNamesOptPrepared, idOpt.isDefined).map(pages => Ok(jsonResultOk(Json.obj("pages" -> pages)))))
        }
      }
    }

  private def isExistsFold(f1: Future[Boolean], code: Int, msg: String)(f2: Future[Result])(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    f1 flatMap (is => if (is) future(BadRequest(jsonResultError(code, msg))) else f2)

  private def register()(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    notAuthorized(fieldString("user_type") { userTypeStr =>
      AccountType.idByStr(userTypeStr).fold(future(BadRequest("Wrong account type: " + userTypeStr))) { accountType =>
        fieldString("login") { login =>
          if (login.matches("[a-z0-9]{3,}")) {
            fieldString("email") { email =>
              isExistsFold(dao.isLoginExists(login), 0, "Login already exists") {
                isExistsFold(dao.isEmailExists(email), 0, "Email already exists") {
                  accountType match {
                    case AccountType.USER =>
                      createAccount("sendgrid.letter", accountType, login, email, None, None, None)(t => Ok(jsonResultOk(t.toJson)))
                    case AccountType.COMPANY =>
                      fieldString("company_name")(companyName =>
                        if (companyName.trim.length < 1) future(BadRequest("Company name should contains one symbol at least")) else
                          fieldString("ITN")(ITN =>
                            if (ITN.trim.length < 1) future(BadRequest("ITN should contains one symbol at least")) else
                              fieldString("IEC")(IEC =>
                                if (IEC.trim.length < 1) future(BadRequest("IEC should contains one symbol at least")) else
                                  createAccount("sendgrid.letter", accountType, login, email, Some(companyName), Some(ITN), Some(IEC))(t => Ok(jsonResultOk(t.toJson))))))
                  }
                }
              }
            }
          } else future(BadRequest("Login contains wrong symbols"))
        }
      }
    })

  protected def logout()(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    super.logout(Ok(jsonResultOk()))

  private def getUser()(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    authorizedNotLocked(user => future(Ok(jsonResultOk(user.toJson))))

  private def getUserInfo()(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    withAccountNameOrId { id =>
      dao.findAccountByIdWithBalances(id)
        .map(_.fold(BadRequest("Can't find account by id " + id))(account => Ok(jsonResultOk(account.toJson))))
    }

  private def postComment()(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    //fieldString("reward_type")(strRewardType => RewardType.idByStr(strRewardType).fold(future(BadRequest("Unknown reward type")))(rewardType =>
    fieldString("content")(content =>
      fieldLongOpt("post_id") { postIdOpt =>

        def createCommentProcess(postId: Long, commentIdOpt: Option[Long]) = authorizedNotLocked { user =>
          if (user.commentsCounter >= AppConstants.COMMENTS_COUNTER_LIMIT) {
            future(BadRequest("Your comments day limit are reached."))
          } else
            dao.createCommentToPostWithCommentsCounterUpdate(postId, commentIdOpt, user.id, content, RewardType.POWER)
              .map(_.fold(BadRequest("Can't create user"))(comment => Ok(jsonResultOk(comment.toJson))))
        }

        postIdOpt.fold(
          fieldLong("comment_id") { commentId =>
            dao.findPostByComment(commentId).flatMap(_.fold(future(BadRequest("Post not found for comment"))) { post =>
              createCommentProcess(post.id, Some(commentId))
            })
          })(postId => createCommentProcess(postId, None))

      }) //))

  private def disableShowBrand()(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    authorizedNotLocked(user => dao.disableShowBrand(user.id).map(t => if (t) Ok(jsonResultOk()) else BadRequest("Couldn't disable show brand for " + user.login)))

  private def updateUserProfileStringField(fieldName: String, f: (Long, String) => Future[Boolean])(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    authorizedNotLocked(user =>
      fieldString(fieldName)(newValue => f(user.id, newValue)
        .map(t => if (t) Ok(jsonResultOk()) else BadRequest("Couldn't update user " + fieldName))))

  private def updateUserProfileLongField(fieldName: String, f: (Long, Long) => Future[Boolean])(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    authorizedNotLocked(user =>
      fieldLong(fieldName)(newValue => f(user.id, newValue)
        .map(t => if (t) Ok(jsonResultOk()) else BadRequest("Couldn't update user " + fieldName))))

  private def updateCompanyITN()(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    updateUserProfileStringField("value", dao.updateCompanyITN)

  private def updateCompanyIEC()(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    updateUserProfileStringField("value", dao.updateCompanyIEC)

  private def updateUserAbout()(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    updateUserProfileStringField("value", dao.updateAccountAbout)

  private def updateUserAvatar()(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    updateUserProfileStringField("value", dao.updateAccountAvatar)

  private def updateUserName()(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    updateUserProfileStringField("value", dao.updateAccountName)

  private def updateUserSurname()(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    updateUserProfileStringField("value", dao.updateAccountSurname)

  private def updateUserSex()(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    updateUserProfileStringField("value", dao.updateAccountSex)

  private def updateUserBirthday()(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    updateUserProfileLongField("value", dao.updateAccountBirthday)

  private def updateUserEducation()(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    updateUserProfileStringField("value", dao.updateAccountEducation)

  private def updateUserBackground()(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    updateUserProfileStringField("value", dao.updateAccountBackground)

  private def createProduct()(implicit request: Request[JsValue], ac: AppContext): Future[Result] = {
    fieldSeqStringOpt("tags") { inTags: Seq[String] =>
      val tags = inTags.map(_.trim.toLowerCase)
      if (tags.length > AppConstants.TAGS_PER_POST_LIMIT) future(BadRequest("You have more than " + AppConstants.TAGS_PER_POST_LIMIT + " tags"))
      else if (tags.exists(_.length < AppConstants.TAG_SIZE_LIMIT)) future(BadRequest("Each tag length should be more than " + (AppConstants.TAG_SIZE_LIMIT - 1))) else
        fieldString("product_name")(name =>
          if (name.trim.length < AppConstants.MIN_TITLE_LENGTH) future(BadRequest("Name length should be " + AppConstants.MIN_TITLE_LENGTH + " at least")) else
            fieldString("about")(about =>
              if (about.trim.length < 1) future(BadRequest("Description can not be null ")) else
                fieldStringOpt("thumbnail")(thumbnailOpt =>
                  fieldInt("value")(value =>
                    if (value < 0) future(BadRequest("value can not be negative")) else
                      fieldInt("alcohol")(alcohol =>
                        if (alcohol < 0 || alcohol > 1000) future(BadRequest("Alcohol should be from 0 to 100")) else
                          fieldStringOpt("address")(addressOpt =>
                            authorizedNotLocked { user =>
                              if (user.postsCounter >= AppConstants.POSTS_COUNTER_LIMIT)
                                future(BadRequest("Your posts day limit are reached."))
                              else {
                                dao.getShortOptionByName(ShortOptions.PRODUCTS_ALLOWED) flatMap {
                                  _.fold(future(BadRequest("Not found option for product allowance posting"))) { option =>
                                    if (option.toBoolean)

                                      dao.getShortOptionByName(ShortOptions.PRODUCTS_PREMODERATION) flatMap {
                                        _.fold(future(BadRequest("Not found option for product moderation"))) { modOpt =>
                                          dao.createProductWithPostsCounterUpdate(user.id, name, about, thumbnailOpt, addressOpt, value, alcohol, tags, modOpt.toBoolean)
                                            .map(_.fold(BadRequest("Can't create product"))(product => Ok(jsonResultOk(product.toJson))))
                                        }
                                      }

                                    else future(BadRequest(jsonResultError(ErrCodes.EC_PRODUCTS_NOT_ALLOWED, ErrCodes.STR_PRODUCTS_NOT_ALLOWED)))
                                  }
                                }
                              }
                            }))))))
    }
  }

  private def createPost()(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    fieldSeqStringOpt("tags") { inTags: Seq[String] =>
      val tags = inTags.map(_.trim.toLowerCase)
      if (tags.length > AppConstants.TAGS_PER_POST_LIMIT) future(BadRequest("You have more than " + AppConstants.TAGS_PER_POST_LIMIT + " tags"))
      else if (tags.exists(_.length < AppConstants.TAG_SIZE_LIMIT)) future(BadRequest("Each tag length should be more than " + (AppConstants.TAG_SIZE_LIMIT - 1))) else
        fieldString("reward_type")(strRewardType => RewardType.idByStr(strRewardType).fold(future(BadRequest("Unknown reward type")))(rewardType =>
          fieldString("post_type")(strPostType => PostType.idByStr(strPostType).fold(future(BadRequest("Unknown post type"))) { postType =>

            def next(moderateOption: Boolean) =
              fieldString("title")(title =>
                if (title.trim.length < AppConstants.MIN_TITLE_LENGTH) future(BadRequest("Title length should be " + AppConstants.MIN_TITLE_LENGTH + " at least")) else
                  fieldString("content")(content =>
                    fieldStringOpt("thumbnail")(thumbnailOpt =>
                      /*if (content.trim.length < AppConstants.MIN_CONTENT_LENGTH) future(BadRequest("Content length should be " + AppConstants.MIN_CONTENT_LENGTH + " at least")) else*/
                      authorizedNotLocked { user =>

                        if (user.postsCounter >= AppConstants.POSTS_COUNTER_LIMIT) {
                          future(BadRequest("Your posts day limit are reached."))
                        } else {
                          if (postType == PostType.REVIEW)
                            fieldLong("product_id") { productId =>
                              dao.isProductExistsById(productId) flatMap { isExists =>
                                if (isExists)
                                  dao.createPostWithPostsCounterUpdate(user.id, Some(productId), title, content, thumbnailOpt, rewardType, postType, None, tags, moderateOption)
                                    .map(_.fold(BadRequest("Can't create user"))(post => Ok(jsonResultOk(post.toJson))))
                                else
                                  future(BadRequest("Product wiht id " + productId + " not exists"))
                              }
                            }
                          else
                            dao.createPostWithPostsCounterUpdate(user.id, None, title, content, thumbnailOpt, rewardType, PostType.ARTICLE, None, tags, moderateOption)
                              .map(_.fold(BadRequest("Can't create user"))(post => Ok(jsonResultOk(post.toJson))))
                        }

                      })))

            if (postType == PostType.PRODUCT) {
              dao.getShortOptionByName(ShortOptions.PRODUCTS_ALLOWED) flatMap {
                _.fold(future(BadRequest("Not found option for product allowance posting"))) { option =>
                  if (option.toBoolean)
                    dao.getShortOptionByName(ShortOptions.PRODUCTS_PREMODERATION) flatMap {
                      _.fold(future(BadRequest("Not found option for product moderation"))) { modOpt =>
                        next(modOpt.toBoolean)
                      }
                    }
                  else future(BadRequest(jsonResultError(ErrCodes.EC_PRODUCTS_NOT_ALLOWED, ErrCodes.STR_PRODUCTS_NOT_ALLOWED)))
                }
              }
            } else if (postType == PostType.ARTICLE) {
              dao.getShortOptionByName(ShortOptions.ARTICLE_ALLOWED) flatMap {
                _.fold(future(BadRequest("Not found option for article allowance posting"))) { option =>
                  if (option.toBoolean)
                    dao.getShortOptionByName(ShortOptions.ARTICLES_PREMODERATION) flatMap {
                      _.fold(future(BadRequest("Not found option for article moderation"))) { modOpt =>
                        next(modOpt.toBoolean)
                      }
                    }
                  else future(BadRequest(jsonResultError(ErrCodes.EC_ARTICLES_NOT_ALLOWED, ErrCodes.STR_ARTICLES_NOT_ALLOWED)))
                }
              }
            } else {
              dao.getShortOptionByName(ShortOptions.REVIEWS_ALLOWED) flatMap {
                _.fold(future(BadRequest("Not found option for reviews allowance posting"))) { option =>
                  if (option.toBoolean)
                    dao.getShortOptionByName(ShortOptions.REVIEWS_PREMODERATION) flatMap {
                      _.fold(future(BadRequest("Not found option for review moderation"))) { modOpt =>
                        next(modOpt.toBoolean)
                      }
                    }
                  else future(BadRequest(jsonResultError(ErrCodes.EC_REVIEWS_NOT_ALLOWED, ErrCodes.STR_REVIEWS_NOT_ALLOWED)))
                }
              }
            }

          })))
    }

  private def isOptionTrue()(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    fieldString("name")(name =>
      dao.getShortOptionByName(name).map(_.fold(BadRequest("Option not found")) { option =>
        if (option.ttype == ShortOptions.TYPE_BOOLEAN)
          Ok(jsonResultOk(Json.obj("result" -> option.toBoolean)))
        else BadRequest("Option type must be boolean!")
      }))

  private def isEmailExists()(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    fieldString("email")(email =>
      dao.findAccountByEmail(email).map(_.fold(Ok(jsonResultOk(Json.obj("result" -> false))))(t => Ok(jsonResultOk(Json.obj("result" -> true))))))

  private def isLoginExists()(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    fieldString("login")(login =>
      dao.findAccountByLogin(login).map(_.fold(Ok(jsonResultOk(Json.obj("result" -> false))))(t => Ok(jsonResultOk(Json.obj("result" -> true))))))

  private def getTransactions()(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    fieldLong("page_id")(pageId =>
      fieldBooleanOpt("is_scheduled")(isSchelduledOpt =>
        authorizedNotLocked(user => dao.getTransactionsByAccountIdWithInfo(user.id, pageId, isSchelduledOpt.getOrElse(false))
          .map(posts => Ok(jsonResultOk(JsArray(posts.map(_.toJson))))))))

  private def getTransactionsPages()(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    fieldBooleanOpt("is_scheduled")(isSchelduledOpt =>
      authorizedNotLocked(user => dao.getTransactionsPagesByAccountIdWithInfo(user.id, isSchelduledOpt.getOrElse(false))
        .map(pages => Ok(jsonResultOk(Json.obj("pages" -> pages))))))

  private def getComments()(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    optionalAuthorized { userOpt =>
      fieldLongOpt("post_id")(_ match {
        case Some(postId) =>
          fieldStringOpt("view_type")(_.fold {
            dao.findCommentsWithAccountsByPostId(postId, userOpt.map(_.id))
              .map(comments =>
                Ok(jsonResultOk(JsArray(comments.map(_.toJson)))))
          } { commentsViewTypeStr =>
            CommentsViewType.idByStr(commentsViewTypeStr) match {
              case Some(CommentsViewType.TREE) =>
                dao.findCommentsWithAccountsByPostId(postId, userOpt.map(_.id))
                  .map(comments => Ok(jsonResultOk(JsArray(models.Comments.buildTree(comments).map(_.toJson)))))
              case _ =>
                dao.findCommentsWithAccountsByPostId(postId, userOpt.map(_.id))
                  .map(comments =>
                    Ok(jsonResultOk(JsArray(comments.map(_.toJson)))))
            }
          })
        case _ =>
          fieldLong("page_id")(pageId =>
            withAccountNameOrId { id =>
              dao.findCommentsWithAccountsForAllAccountPosts(id, pageId, userOpt.map(_.id))
                .map(comments => Ok(jsonResultOk(JsArray(comments.map(_.toJson)))))
            })
      })
    }

  private def getCommentsPages()(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    fieldLongOpt("post_id")(_ match {
      case Some(postId) => future(BadRequest("Not implemented yet"))
      case _ =>
        fieldLong("page_id")(pageId =>
          withAccountNameOrId { id =>
            dao.findCommentsPagesWithAccountsForAllAccountPosts(id)
              .map(pages => Ok(jsonResultOk(Json.obj("pages" -> pages))))
          })
    })

  private def like()(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    authorizedNotLocked { user =>

      def createLikeProcess(postId: Long, commentIdOpt: Option[Long]) =
        if (user.likesCounter >= AppConstants.LIKES_COUNTER_LIMIT) {
          future(BadRequest("Your likes day limit are reached."))
        } else
          dao.createLikeToPostWithLikesCounterUpdate(user.id, postId, commentIdOpt)
            .map(_.fold(BadRequest("Can't create like"))(like => Ok(jsonResultOk(like.toJson))))

      fieldLongOpt("post_id")(_.fold(
        fieldLong("comment_id") { commentId =>
          dao.findCommentById(commentId) flatMap (_.fold(future(BadRequest("Comment not found " + commentId))) { comment =>
            if (comment.ownerId == user.id) future(BadRequest("You can't like your comments")) else {
              dao.isAlreadyLikeCommentByAccount(user.id, commentId) flatMap { is =>
                if (is) future(BadRequest("Comment already liked by user " + user.id)) else {
                  createLikeProcess(comment.postId, Some(commentId))
                }
              }
            }
          })
        }) { postId =>
          dao.findPostById(postId) flatMap (_.fold(future(BadRequest("Can't find post by id " + postId))) { post =>
            if (post.ownerId == user.id) future(BadRequest("You can't like your posts")) else {
              dao.isAlreadyLikePostByAccount(user.id, postId) flatMap { is =>
                if (is) future(BadRequest("Post already liked by user " + user.id)) else {
                  createLikeProcess(postId, None)
                }
              }
            }
          })
        })
    }

  private def transfer()(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    fieldString("currency")(currencyStr =>
      CurrencyType.idByStr(currencyStr).fold(future(BadRequest("Specified currency type not supported"))) { currency =>
        withAccountNameOrId { id =>
          dao.findAccountById(id).flatMap(_.fold(future(BadRequest("Can't find user with id " + id))) { toAccount =>
            fieldLong("value")(value => authorizedNotLocked { user =>
              enoughFunds(user.id, currency, value) {
                fieldStringOpt("msg")(msgOpt =>
                  dao.transfer(user.id, toAccount.id, currency, value, msgOpt)
                    .map(_.fold(BadRequest("Can't create transaction"))(tx => Ok(jsonResultOk(tx.toJson)))))
              }
            })
          })
        }
      })

  private def existsProductInCompany(productId: Long, accountId: Long)(f: Future[Result]): Future[Result] =
    dao.existsProductInCompany(productId, accountId).flatMap { exists =>
      if (exists) f else future(BadRequest("Company with id " + accountId + " not have product with id " + productId))
    }

  private def enoughFunds(userId: Long, currencyType: Int, value: Long)(f: Future[Result]): Future[Result] =
    dao.enoughFunds(userId, currencyType, value).flatMap { enough =>
      if (value <= 0) future(BadRequest("Value must be positive!")) else {
        if (enough) f else future(BadRequest("You haven't enough funds"))
      }
    }

  private def promote()(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    fieldLong("value")(value => fieldLong("post_id")(postId =>
      dao.findPostById(postId).flatMap(_.fold(future(BadRequest("Can't find post with id " + postId)))(post =>
        authorizedNotLocked { user =>
          enoughFunds(user.id, CurrencyType.DOLLAR, value) {
            fieldStringOpt("msg")(msgOpt =>
              dao.promote(user.id, post, value, msgOpt)
                .map(_.fold(BadRequest("Can't create transaction"))(tx => Ok(jsonResultOk(tx.toJson)))))
          }
        }))))

  private def shortInContext(f: Future[Result])(implicit request: Request[JsValue], ac: AppContext) =
    fieldBooleanOpt("short") { isShortOpt =>
      ac.props("short") = isShortOpt.getOrElse(false)
      f
    }

  private def getPost()(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    fieldLong("post_id")(postId =>
      optionalAuthorized { userOpt =>
        val userIdOpt = userOpt.map(_.id)
        shortInContext {
          // Needs to check post if exists before!
          dao.findPostWithAccountByPostId(postId, userIdOpt)
            .map(_.fold(BadRequest("Post not found " + postId))(post => Ok(jsonResultOk(post.toJson))))
        }
      })

  private def getProduct()(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    fieldLong("product_id")(productId =>
      optionalAuthorized { userOpt =>
        val userIdOpt = userOpt.map(_.id)
        shortInContext {
          dao.findProductWithAccountByProductId(productId, userIdOpt)
            .map(_.fold(BadRequest("Product not found " + productId))(product => Ok(jsonResultOk(product.toJson))))
        }
      })

  private def search()(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    fieldLong("page_id")(pageId => fieldString("text")(text =>
      optionalAuthorized(optAccount =>
        dao.findPostsWithAccountsByText(pageId, text, optAccount.map(_.id)) map { posts =>
          Ok(jsonResultOk(JsArray(posts.map(_.toJson))))
        })))

  private def getTags()(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    fieldLong("page_id")(pageId =>
      dao.getTagsPage(pageId) map (tags => Ok(jsonResultOk(JsArray(tags.map(_.toJson))))))

  private def getTagsPages()(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    fieldLong("page_id")(pageId =>
      dao.getTagsPages() map (pages => Ok(jsonResultOk(Json.obj("pages" -> pages)))))

  private def approveTransaction()(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    fieldLong("transaction_id")(txId => authorizedNotLocked(user =>
      dao.findTransactionById(txId).flatMap(_.fold(future(BadRequest("Transaction not found"))) { tx =>
        if (tx.scheduled.isEmpty) future(BadRequest("Transaction not schelduled")) else {
          if (tx.toType == TargetType.ACCOUNT) {
            tx.toId.fold(future(BadRequest("Transaction not owned"))) { toId =>
              if (toId == user.id) {
                if (tx.scheduled.get < System.currentTimeMillis) future(BadRequest("Tx scheldule time not reached!")) else {
                  dao.approveTransaction(txId)
                    .map(_.fold(BadRequest("Can't approve transaction"))(tx => Ok(jsonResultOk(tx.toJson))))
                }
              } else future(BadRequest("You not authorized to approve this transaction!"))
            }
          } else future(BadRequest("Not transaction to user!"))
        }
      })))

  private def exchange()(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    fieldString("currency_from")(currencyStr =>
      CurrencyType.idByStr(currencyStr).fold(future(BadRequest("Specified currency from type not supported")))(currencyFrom =>
        fieldString("currency_to")(currencyStr =>
          CurrencyType.idByStr(currencyStr).fold(future(BadRequest("Specified currency to type not supported")))(currencyTo =>
            fieldLong("value")(value => authorizedNotLocked { user =>
              enoughFunds(user.id, currencyFrom, value) {
                (currencyFrom, currencyTo) match {
                  case (CurrencyType.DOLLAR, CurrencyType.DOLLAR) => future(BadRequest("Wrong pair"))
                  case (CurrencyType.POWER, CurrencyType.POWER)   => future(BadRequest("Wrong pair"))
                  case (CurrencyType.TOKEN, CurrencyType.TOKEN)   => future(BadRequest("Wrong pair"))
                  case (CurrencyType.DOLLAR, CurrencyType.POWER)  => future(BadRequest("Wrong pair"))
                  case (CurrencyType.POWER, CurrencyType.DOLLAR)  => future(BadRequest("Wrong pair"))
                  case _ => dao.exchange(user, currencyFrom, currencyTo, value)
                    .map(r => if (r) Ok(jsonResultOk()) else BadRequest("Error occurs during exhange"))
                }
              }
            })))))

  private def getReviews()(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    fieldLong("page_id")(pageId => fieldStringOpt("filter")(filterName => fieldLong("product_id")(productId =>
      optionalAuthorized { optAccount =>
        shortInContext {
          //withAccountNameOrIdSingleOpt(idOpt =>
          dao.findReviewsWithAccountsByCategoryTagNames(optAccount.map(_.id), productId, pageId) map { reviews =>
            Ok(jsonResultOk(JsArray(reviews.map(_.toJson))))
          } //)
        }
      })))

  private def getReviewsPages()(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    fieldStringOpt("filter")(filterName => fieldLong("product_id")(productId =>
      shortInContext {
        //withAccountNameOrIdSingleOpt(idOpt =>
        optionalAuthorized { optAccount =>
          dao.findReviewsPagesWithAccountsByCategoryTagNames(productId) map { pages =>
            Ok(jsonResultOk(Json.obj("pages" -> pages)))
          } //)
        }
      }))

  private def getProducts()(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    fieldLong("page_id")(pageId => fieldStringOpt("filter") { filterName =>
      optionalAuthorized { optAccount =>
        shortInContext {
          fieldSeqStringOptOpt("tags") { tagNamesOpt: Option[Seq[String]] =>
            val tagNamesOptPrepared = tagNamesOpt.map(_.map(_.trim.toLowerCase))
            if (tagNamesOptPrepared.isDefined && tagNamesOptPrepared.get.length > AppConstants.TAGS_PER_POST_LIMIT) future(BadRequest("You have more than " + AppConstants.TAGS_PER_POST_LIMIT + " tags"))
            else if (tagNamesOptPrepared.isDefined && tagNamesOptPrepared.get.exists(_.length < AppConstants.TAG_SIZE_LIMIT)) future(BadRequest("Each tag length should be more than " + (AppConstants.TAG_SIZE_LIMIT - 1))) else {
              val userIdOpt = optAccount.map(_.id)
              withAccountNameOrIdSingleOpt(idOpt =>
                dao.findProductsWithAccountsByCategoryTagNames(
                  userIdOpt, idOpt, filterName, pageId, tagNamesOptPrepared) map { products =>
                  Ok(jsonResultOk(JsArray(products.map(_.toJson))))
                })
            }
          }
        }
      }
    })

  private def getProductsPages()(implicit request: Request[JsValue], ac: AppContext): Future[Result] =
    fieldStringOpt("filter") { filterName =>
      optionalAuthorized { optAccount =>
        shortInContext {
          fieldSeqStringOptOpt("tags") { tagNamesOpt: Option[Seq[String]] =>
            val tagNamesOptPrepared = tagNamesOpt.map(_.map(_.trim.toLowerCase))
            if (tagNamesOptPrepared.isDefined && tagNamesOptPrepared.get.length > AppConstants.TAGS_PER_POST_LIMIT) future(BadRequest("You have more than " + AppConstants.TAGS_PER_POST_LIMIT + " tags"))
            else if (tagNamesOptPrepared.isDefined && tagNamesOptPrepared.get.exists(_.length < AppConstants.TAG_SIZE_LIMIT)) future(BadRequest("Each tag length should be more than " + (AppConstants.TAG_SIZE_LIMIT - 1))) else {
              val userIdOpt = optAccount.map(_.id)
              withAccountNameOrIdSingleOpt(idOpt =>
                dao.findProductsPagesWithAccountsByCategoryTagNames(userIdOpt, idOpt, filterName, tagNamesOptPrepared) map { pages =>
                  Ok(jsonResultOk(Json.obj("pages" -> pages)))
                })
            }
          }
        }
      }
    }

}

