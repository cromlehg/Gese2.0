package models.daos

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Random

import org.mindrot.jbcrypt.BCrypt

import controllers.AppConstants
import controllers.RewardLogic
import javax.inject.Inject
import models.AccountStatus
import models.CurrencyType
import models.PostsFilter
import models.Roles
import models.TargetType
import models.TxState
import models.TxType
import models.Account
import models.UserStatus
import play.api.db.slick.DatabaseConfigProvider
import play.api.db.slick.HasDatabaseConfigProvider
import controllers.AppContext
import models.ContentType
import models.BalanceType
import models.ItemStatus
import models.AccountType
import models.PostType
import sun.net.ftp.FtpClient.TransferType
import models.AccountLevel
import models.MarketingCampaignStatus
import models.ModerateStatus

/**
 *
 * Queries with SlickBUG should be replace leftJoin with for comprehesive. Bug:
 * "Unreachable reference to after resolving monadic joins"
 *
 */

// inject this
// conf: play.api.Configuration,
// and then get conf value
// conf.underlying.getString(Utils.meidaPath)
class DAO @Inject() (protected val dbConfigProvider: DatabaseConfigProvider)(implicit ec: ExecutionContext)
  extends DBTableDefinitions with HasDatabaseConfigProvider[slick.jdbc.JdbcProfile] with TraitDTOToModel {

  import profile.api._

  val pageSize = 17

  val maxLikesView = 10

  def getTagsPages(): Future[Int] =
    db.run(tags.size.result) map pages

  def pages(size: Int): Int = pages(size, pageSize)

  def pages(size: Int, pageSize: Int): Int = {
    if (size == 0) 0 else {
      val fSize = size / pageSize
      if (fSize * pageSize < size) fSize + 1 else fSize
    }
  }

  def removePostRaw(postId: Long) =
    marketingCampaigns.filter(_.productId === postId).delete.andThen(
      batches.filter(_.productId === postId).map(_.id).result flatMap { batchIds =>
        DBIO.sequence(batchIds map { batchId =>
          items.filter(_.batchId === batchId).delete
        }).andThen(batches.filter(_.productId === postId).delete)
      }).andThen(comments.filter(_.postId === postId).delete)
      .andThen(posts.filter(_.productId === postId).map(_.id).result flatMap { postsIds =>
        DBIO.sequence(postsIds map { childPostId =>
          comments.filter(_.postId === childPostId).delete
        }).andThen(posts.filter(_.productId === postId).delete)
      })
      .andThen(posts.filter(_.id === postId).delete)

  def removePost(postId: Long) =
    db.run(removePostRaw(postId).transactionally).map(_ > 0)

  def setModerateStatusToPost(postId: Long, modStatus: Int) =
    db.run(posts.filter(_.id === postId).map(_.moderateStatus).update(modStatus).transactionally).map(_ == 1)

  def setAccountStatus(accountId: Long, status: Int) =
    db.run(accounts.filter(_.id === accountId).map(_.userStatus).update(status).transactionally).map(_ == 1)

  // TODO
  def removeAccount(accountId: Long): Future[Boolean] =
    Future(false)

  def getAccounts(filterOpt: Option[String], pageId: Int): Future[Seq[models.Account]] =
    filterOpt.fold {
      db.run(accounts.sortBy(_.id.desc).drop(if (pageId > 0) pageSize * (pageId - 1) else 0).take(pageSize).result) map (_ map accountFrom)
    } { filter =>
      db.run(accounts.filter(_.login like ("%" + filter + "%")).sortBy(_.id.desc).drop(if (pageId > 0) pageSize * (pageId - 1) else 0).take(pageSize).result) map (_ map accountFrom)
    }

  def getAccountsPagesCount(filterOpt: Option[String]): Future[Int] =
    filterOpt.fold {
      db.run(accounts.length.result).map { r =>
        pages(r, pageSize.toInt)
      }
    } { filter =>
      db.run(accounts.filter(_.login like ("%" + filter + "%")).length.result).map { r =>
        pages(r, pageSize.toInt)
      }
    }

  def isExistsBottle(code: String): Future[Boolean] = {
    val prepCode = code.trim.toLong
    db.run(items.filter(t => t.id === prepCode).exists.result)
  }

  def getAdminPosts(pageId: Int): Future[Seq[models.Post]] =
    db.run(posts.sortBy(_.id.desc).drop(if (pageId > 0) pageSize * (pageId - 1) else 0).take(pageSize).result) map (_ map postFrom)

  def getAdminPostsPagesCount(): Future[Int] =
    db.run(posts.length.result).map { r =>
      pages(r, pageSize.toInt)
    }

  def getAdminModeratePosts(pageId: Int): Future[Seq[models.Post]] =
    db.run(posts.filter(_.moderateStatus === ModerateStatus.NEED_A_MODERATION).sortBy(_.id.desc).drop(if (pageId > 0) pageSize * (pageId - 1) else 0).take(pageSize).result) map (_ map postFrom)

  def getAdminModeratePostsPagesCount(): Future[Int] =
    db.run(posts.filter(_.moderateStatus === ModerateStatus.NEED_A_MODERATION).length.result).map { r =>
      pages(r, pageSize.toInt)
    }

  def getShortOptions(): Future[Seq[models.ShortOption]] =
    db.run(shortOptions.result) map (_ map shortOptionFrom)

  def getShortOptionByName(name: String): Future[Option[models.ShortOption]] =
    db.run(shortOptions.filter(_.name === name).result.headOption) map (_ map shortOptionFrom)

  def updateShortOptionByName(name: String, value: String): Future[Option[models.ShortOption]] =
    db.run(shortOptions.filter(_.name === name).map(_.value).update(value).map(_ > 1)
      .flatMap(_ => shortOptions.filter(_.name === name).result.headOption)) map (_ map shortOptionFrom)

  def getNotificationTasksWithEmailAndProduct(pageId: Long): Future[Seq[models.ScheduledTask]] = {
    val now = System.currentTimeMillis()
    db.run(scheduledTasks
      .filter(t => t.taskType === models.TaskType.NOTIFICATIONS && t.executed.isEmpty && t.planned < now)
      .drop(if (pageId > 0) pageSize * (pageId - 1) else 0).take(pageSize)
      .join(accounts).on(_.accountId === _.id)
      .join(posts).on { case ((task, account), product) => task.productId === product.id }
      .result) map {
      _.map {
        case ((task, account), product) =>
          val modelTask = taskFrom(task)
          modelTask.emailOpt = Some(account.email)
          modelTask.productOpt = Some(postFrom(product))
          modelTask
      }
    }
  }

  def notificationsSended(ids: Seq[Long]): Future[Int] =
    db.run(scheduledTasks.filter(_.id inSet ids).map(_.executed).update(Some(System.currentTimeMillis())).transactionally)

  def getNotificationsPages(): Future[Int] = {
    val now = System.currentTimeMillis()
    db.run(scheduledTasks
      .filter(t => t.taskType === models.TaskType.NOTIFICATIONS && t.executed.isEmpty && t.planned < now)
      .size.result) map pages
  }

  def getTagsPage(pageId: Long): Future[Seq[models.Tag]] =
    db.run(tags.sortBy(_.id.desc).drop(if (pageId > 0) pageSize * (pageId - 1) else 0).take(pageSize).result) map (_ map tagFrom)

  def getAccountsPage(pageId: Long): Future[Seq[models.Account]] =
    db.run(accounts.sortBy(_.id.desc).drop(if (pageId > 0) pageSize * (pageId - 1) else 0).take(pageSize).result) map (_ map accountFrom)

  def getAccountsPages(): Future[Int] =
    db.run(accounts.size.result) map pages

  def findAccountById(id: Long) =
    getAccountFromQuery(accounts.filter(_.id === id))

  def findAccountByEmail(email: String): Future[Option[Account]] =
    getAccountFromQuery(accounts.filter(_.email === email))

  def isPostOwner(postId: Long, accountId: Long): Future[Boolean] =
    db.run(posts.filter(t => t.id === postId && t.ownerId === accountId).exists.result)

  def findAccountByLoginOrEmailWithBalances(loginOrElamil: String): Future[Option[Account]] = {
    db.run {
      for {
        dbaccount <- accounts.filter(u => u.login === loginOrElamil || u.email === loginOrElamil).result.head
        balanceTokens <- filterCurrentAccountBalanceValue(dbaccount.id, CurrencyType.TOKEN).result.headOption
        balanceDollars <- filterCurrentAccountBalanceValue(dbaccount.id, CurrencyType.DOLLAR).result.headOption
        balancePower <- filterCurrentAccountBalanceValue(dbaccount.id, CurrencyType.POWER).result.headOption
      } yield (dbaccount, balanceTokens, balanceDollars, balancePower)
    } map {
      case (dbaccount, balanceTokens, balanceDollars, balancePower) =>
        val account = accountFrom(dbaccount)
        account.balanceTokenOpt = balanceTokens
        account.balanceDollarOpt = balanceDollars
        account.balancePowerOpt = balancePower
        Some(account)
    }
  }

  def findAccountByLoginOrEmail(loginOrElamil: String): Future[Option[Account]] =
    updateAccountWithRoles(getAccountFromQuery(accounts.filter(u => u.login === loginOrElamil || u.email === loginOrElamil)))

  def findAccountIdByLoginOrEmail(loginOrElamil: String): Future[Option[Long]] =
    getAccountFromQuery(accounts.filter(u => u.login === loginOrElamil || u.email === loginOrElamil)).map(_.map(_.id))

  def findAccountByLogin(login: String): Future[Option[Account]] =
    getAccountFromQuery(accounts.filter(_.login === login))

  def findBalances(pageId: Long): Future[Seq[models.Balance]] =
    db.run(balances.sortBy(_.id.desc).drop(if (pageId > 0) pageSize * (pageId - 1) else 0).take(pageSize).result) map (_ map balanceFrom)

  def findAccountByIdWithBalances(id: Long): Future[Option[Account]] = {
    db.run {
      for {
        dbaccount <- accounts.filter(_.id === id).result.head
        balanceTokens <- filterCurrentAccountBalanceValue(dbaccount.id, CurrencyType.TOKEN).result.headOption
        balanceDollars <- filterCurrentAccountBalanceValue(dbaccount.id, CurrencyType.DOLLAR).result.headOption
        balancePower <- filterCurrentAccountBalanceValue(dbaccount.id, CurrencyType.POWER).result.headOption
      } yield (dbaccount, balanceTokens, balanceDollars, balancePower)
    } map {
      case (dbaccount, balanceTokens, balanceDollars, balancePower) =>
        val account = accountFrom(dbaccount)
        account.balanceTokenOpt = balanceTokens
        account.balanceDollarOpt = balanceDollars
        account.balancePowerOpt = balancePower
        Some(account)
    }
  }

  def findAccountByLoginWithBalances(login: String): Future[Option[Account]] = {
    db.run {
      for {
        dbaccount <- accounts.filter(_.login === login).result.head
        balanceTokens <- filterCurrentAccountBalanceValue(dbaccount.id, CurrencyType.TOKEN).result.headOption
        balanceDollars <- filterCurrentAccountBalanceValue(dbaccount.id, CurrencyType.DOLLAR).result.headOption
        balancePower <- filterCurrentAccountBalanceValue(dbaccount.id, CurrencyType.POWER).result.headOption
      } yield (dbaccount, balanceTokens, balanceDollars, balancePower)
    } map {
      case (dbaccount, balanceTokens, balanceDollars, balancePower) =>
        val account = accountFrom(dbaccount)
        account.balanceTokenOpt = balanceTokens
        account.balanceDollarOpt = balanceDollars
        account.balancePowerOpt = balancePower
        Some(account)
    }
  }

  def getCampaings(
    accountIdOpt: Option[Long],
    productIdOpt: Option[Long],
    isCurrent:    Boolean,
    pageId:       Long): Future[Seq[models.MarketingCampaign]] = {
    db.run {
      {
        val mcFilteredByProdId = productIdOpt match {
          case Some(productId) => marketingCampaigns.filter(_.productId === productId)
          case _               => marketingCampaigns
        }

        val timestamp = System.currentTimeMillis()

        val mcFilteredByCurrentOption =
          if (isCurrent)
            mcFilteredByProdId.filter(t => t.start <= timestamp && t.end > timestamp && t.count > 0 && t.status === MarketingCampaignStatus.ACTIVE)
          else
            mcFilteredByProdId

        accountIdOpt match {
          case Some(accountId) => mcFilteredByCurrentOption.filter(_.productId in posts.filter(t => t.ownerId === accountId && t.postType === PostType.PRODUCT).map(_.id))
          case _               => mcFilteredByCurrentOption
        }

      }.join(posts.map(t => (t.id, t.title))).on(_.productId === _._1)
        .sortBy { case (batch, prodProp) => batch.id.desc }
        .drop(if (pageId > 0) pageSize * (pageId - 1) else 0)
        .take(pageSize).result
    }.map(_.map {
      case (batch, prodProp) =>
        val model = marketingCampaignFrom(batch)
        model.productNameOpt = Some(prodProp._2)
        model
    })
  }

  def processRewardForItemByCode(accountId: Long, code: String, rate: Int): Future[Option[models.Balance]] = {
    val query = for {
      account <- accounts.filter(_.id === accountId).result.head
      balanceActualOpt <- getNotBoughtItemWithBatchByCodeDB(code) flatMap (_ match {
        case Some((item, batch)) =>
          val batchReward = (batch.price * AccountLevel.kByRate(account.rate)).toLong
          addRewardOpt(
            models.TxType.REDEEM_REWARD_FROM_BATCH,
            account.id,
            batchReward,
            models.CurrencyType.DOLLAR,
            None,
            TargetType.BATCH,
            Some(batch.id),
            None,
            None) flatMap (_ match {
              case Some(batchTx) =>
                batches.filter(_.id === batch.id).map(t => t.count).update(batch.count - 1) flatMap (_ match {
                  case 1 => updateCurrentBatchBalance(batch.id, CurrencyType.DOLLAR, -batchReward) flatMap (_ match {
                    case 1 =>
                      getActiveCampaingsForProductDB(batch.productId) flatMap { campaings =>
                        if (campaings.length > 0)
                          DBIO.sequence(campaings map { campaing =>
                            val campaingReward = (campaing.price * AccountLevel.kByRate(account.rate)).toLong
                            addRewardOpt(
                              models.TxType.REDEEM_REWARD_FROM_CAMPAING,
                              account.id,
                              campaingReward,
                              models.CurrencyType.DOLLAR,
                              None,
                              TargetType.CAMPAIGN,
                              Some(campaing.id),
                              None,
                              None) flatMap (_ match {
                                case Some(campaingTx) =>
                                  marketingCampaigns.filter(_.id === campaing.id).map(_.count).update(campaing.count - 1) flatMap (_ match {
                                    case 1 => updateCurrentCampaingBalance(batch.id, CurrencyType.DOLLAR, -campaingReward)
                                    case _ => DBIO.successful(0)
                                  })
                                case _ => DBIO.successful(0)
                              })
                          }).flatMap { _ => rateUpdate(item, batch, account, rate) }
                        else rateUpdate(item, batch, account, rate)
                      }
                    case _ => DBIO.successful(None)
                  })
                  case _ => DBIO.successful(None)
                })
              case _ => DBIO.successful(None)
            })
        case _ => DBIO.successful(None)
      })
    } yield balanceActualOpt
    db.run(query.transactionally).map(_ map balanceFrom)
  }

  def rateUpdate(item: DBItem, batch: DBBatch, account: DBAccount, rate: Int) = {

    items.filter(_.id === item.id)
      .map(t => (t.bought, t.buyerId, t.status))
      .update(Some(System.currentTimeMillis()), Some(account.id), ItemStatus.BOUGHT) flatMap (_ match {
        case 1 =>

          def accountUpdate = {
            val boughtCount = account.bought + 1
            val newRate = AccountLevel.getRateByBoughtCount(boughtCount)
            accounts.filter(_.id === account.id).map(t => (t.bought, t.rate)).update(boughtCount, newRate) flatMap (_ match {
              case 1 => findCurrentAccountBalanceOpt(account.id, CurrencyType.DOLLAR)
              case _ => DBIO.successful(None)
            })
          }

          if (rate > 0)
            posts.filter(t => t.id === batch.productId).map(t => (t.rate, t.rateCount)).result.head flatMap {
              case (oldRate, oldRateCount) =>
                posts.filter(t => t.id === batch.productId).map(t => (t.rate, t.rateCount)).update(oldRate + rate, oldRateCount + 1) flatMap (_ match {
                  case 1 => accountUpdate

                  case _ => DBIO.successful(None)
                })
            }
          else accountUpdate

        case _ => DBIO.successful(None)
      })

  }

  def getBatchesWithBalances(
    accountId:    Long,
    productIdOpt: Option[Long],
    pageId:       Long): Future[Seq[models.Batch]] =
    db.run {
      (productIdOpt match {
        case Some(productId) => batches.filter(_.productId === productId)
        case _               => batches
      }).filter(_.productId in posts.filter(t => t.postType === PostType.PRODUCT && t.ownerId === accountId).map(_.id))
        .join(balances.filter(t =>
          t.balanceType === BalanceType.CURRENT &&
            t.currencyId === CurrencyType.DOLLAR &&
            t.ownerType === TargetType.BATCH)).on(_.id === _.ownerId)
        .join(posts.map(t => (t.id, t.title))).on { case ((batch, balance), prodProp) => batch.productId === prodProp._1 }
        .sortBy { case ((batch, balance), prodProp) => batch.id.desc }
        .drop(if (pageId > 0) pageSize * (pageId - 1) else 0)
        .take(pageSize).result
    }.map(_.map {
      case ((batch, balance), prodProp) =>
        val batchModel = batchFrom(batch)
        batchModel.productNameOpt = Some(prodProp._2)
        batchModel.balanceOpt = Some(balanceFrom(balance))
        batchModel
    })

  def getBatches(
    accountId:    Long,
    productIdOpt: Option[Long],
    pageId:       Long): Future[Seq[models.Batch]] =
    db.run {
      (productIdOpt match {
        case Some(productId) => batches.filter(_.productId === productId)
        case _               => batches
      }).filter(_.productId in posts.filter(t => t.postType === PostType.PRODUCT && t.ownerId === accountId).map(_.id)).join(posts.map(t => (t.id, t.title))).on(_.productId === _._1)
        .sortBy { case (batch, prodProp) => batch.id.desc }
        .drop(if (pageId > 0) pageSize * (pageId - 1) else 0)
        .take(pageSize).result
    }.map(_.map {
      case (batch, prodProp) =>
        val batchModel = batchFrom(batch)
        batchModel.productNameOpt = Some(prodProp._2)
        batchModel
    })

  def createBatch(productId: Long, price: Long, count: Int, balance: Long): Future[Option[models.Batch]] = {
    val timestamp = System.currentTimeMillis()
    val query = for {
      productOwnerId <- posts.filter(t => t.id === productId && t.postType === PostType.PRODUCT).map(_.ownerId).result.head
      account <- accounts.filter(_.id === productOwnerId).result.head
      accountBalance <- filterCurrentAccountBalanceValue(account.id, CurrencyType.DOLLAR).result.head
      batchOpt <- if (accountBalance < balance) DBIO.successful(None) else
        ((batches returning batches.map(_.id) into ((v, id) => Some(v.copy(id = id)))) += new models.daos.DBBatch(
          0,
          productId,
          timestamp,
          count,
          price)) flatMap (_ match {
          case Some(batch) =>
            filterCurrentAccountBalance(account.id, CurrencyType.DOLLAR)
              .map(t => (t.updated, t.value))
              .update(timestamp, accountBalance - balance)
              .flatMap(_ => (balances += new DBBalance(
                0,
                batch.id,
                TargetType.BATCH,
                CurrencyType.DOLLAR,
                timestamp,
                BalanceType.CURRENT,
                balance))
                .flatMap(_ => (transactions += new models.daos.DBTransaction(
                  0,
                  timestamp,
                  None,
                  Some(timestamp),
                  TargetType.ACCOUNT,
                  TargetType.BATCH,
                  Some(account.id),
                  Some(batch.id),
                  None,
                  None,
                  None,
                  None,
                  None,
                  None,
                  TxType.CREATE_BATCH_DEPOSIT,
                  None,
                  TxState.APPROVED,
                  CurrencyType.DOLLAR,
                  balance)).flatMap(_ => DBIO.successful(Some(batch)))))
          case _ => DBIO.successful(None)
        })
      _ <- batchOpt match {
        case Some(batch) => items ++= (1 to count).map(t =>
          new DBItem(
            0,
            batch.id,
            BCrypt.hashpw(t.toString + batch.id.toString + timestamp + Random.nextDouble(), BCrypt.gensalt())
              .replaceAll("\\.", "s")
              .replaceAll("\\\\", "d")
              .replaceAll("\\$", "g").toList.map(_.toInt.toHexString).mkString.substring(0, 99),
            ItemStatus.READY,
            None,
            None))
        case _ => DBIO.successful(None)
      }
    } yield batchOpt
    db.run(query.transactionally) map (_.map(batchFrom))
  }

  def getProductIdAndNamesByLoing(userNameOpt: Option[String], name: String, pageId: Long): Future[Seq[(Long, String)]] = {
    userNameOpt.fold(getProductIdAndNames(None, name, pageId)) { userName =>
      findAccountByLogin(userName) flatMap (_.fold(Future.successful(Seq.empty[(Long, String)])) { account =>
        getProductIdAndNames(Some(account.id), name, pageId)
      })
    }
  }

  def getProductIdAndNames(accountIdOpt: Option[Long], name: String, pageId: Long): Future[Seq[(Long, String)]] =
    db.run {
      (accountIdOpt match {
        case Some(accountId) => posts.filter(t => t.ownerId === accountId && t.postType === PostType.PRODUCT)
        case _               => posts
      }).filter(_.title like (name + "%")).map(t => (t.id, t.title))
        .sortBy(_._1.desc)
        .drop(if (pageId > 0) pageSize * (pageId - 1) else 0)
        .take(pageSize).result
    }

  def getBatchCodes(batchId: Long): Future[Seq[(Long, String)]] =
    db.run(items.filter(t => t.batchId === batchId && t.bought.isEmpty).map(t => (t.id, t.code)).result)

  def isAccountOwnerOfBatch(accountId: Long, batchId: Long): Future[Boolean] =
    db.run(batches.filter(t => t.id === batchId && t.productId.in(posts.filter(t => t.ownerId === accountId && t.postType === PostType.PRODUCT).map(_.id))).exists.result)

  def setCommentStatus(commentId: Long, status: Int): Future[Boolean] =
    db.run(comments.filter(_.id === commentId).map(_.status).update(status).transactionally.map(_ > 0))

  def isAccountOwnerOfComment(accountId: Long, commentId: Long): Future[Boolean] =
    db.run(comments.filter(t => t.id === commentId && t.ownerId === accountId).exists.result)

  def isLoginExists(login: String): Future[Boolean] =
    db.run(accounts.filter(t => t.login === login.trim.toLowerCase || t.email === login).exists.result)

  def isEmailExists(email: String): Future[Boolean] =
    db.run(accounts.filter(_.email === email.trim.toLowerCase).exists.result)

  def findSessionByAccountIdSessionKeyAndIP(userId: Long, ip: String, sessionKey: String): Future[Option[models.Session]] =
    getSessionFromQuery(sessions.filter(s => s.userId === userId && s.ip === ip && s.sessionKey === sessionKey))

  def findProductIdByName(productName: String): Future[Option[Long]] =
    db.run(posts.filter(t => t.title === productName && t.postType === PostType.PRODUCT).map(_.id).result.headOption)

  def findProductWithAccountByProductId(productId: Long, actorIdOpt: Option[Long]): Future[Option[models.Post]] =
    actorIdOpt.fold(findProductWithAccountByProductId(productId))(t => findProductWithAccountByProductId(productId, t))

  def findPostWithAccountByPostId(postId: Long, actorIdOpt: Option[Long]): Future[Option[models.Post]] =
    actorIdOpt.fold(findPostWithAccountByPostId(postId))(t => findPostWithAccountByPostId(postId, t))

  def findPostWithAccountByPostId(postId: Long): Future[Option[models.Post]] =
    db.run(for {
      dbPost <- posts.filter(_.id === postId).result.head
      dbAccount <- accounts.filter(_.id === dbPost.ownerId).result.head
    } yield (dbPost, dbAccount)) map {
      case (dbPost, dbAccount) =>
        val post = postFrom(dbPost)
        post.ownerOpt = Some(accountFrom(dbAccount))
        Some(post)
    } flatMap (
      _ match {
        case Some(post) => findLikes(Seq(post.id), TargetType.POST).map { likes =>
          post.likes = likes
          post
        } flatMap (post => findProductsTitles(Seq(post).filter(_.productId.isDefined).map(_.productId.get)).map {
          case idTitleSeq =>
            Seq(post).filter(_.productId.isDefined).foreach(tpost => tpost.titleRefOpt = idTitleSeq.find(_._1 == tpost.productId.get).map(_._2))
            Some(post)
        })
        case _ => Future.successful(None)
      })

  def findProductWithAccountByProductId(productId: Long): Future[Option[models.Post]] =
    db.run(for {
      dbProduct <- posts.filter(t => t.id === productId && t.postType === PostType.PRODUCT).result.head
      dbAccount <- accounts.filter(_.id === dbProduct.ownerId).result.head
    } yield (dbProduct, dbAccount)) map {
      case (dbProduct, dbAccount) =>
        val product = postFrom(dbProduct)
        product.ownerOpt = Some(accountFrom(dbAccount))
        Some(product)
    } flatMap (
      _ match {
        case Some(product) => findLikes(Seq(product.id), TargetType.POST).map { likes =>
          product.likes = likes
          Some(product)
        }
        case _ => Future.successful(None)
      })

  def findLikes(targetIds: Seq[Long], targetType: Int): Future[Seq[models.Like]] = {
    db.run((likes.filter(_.targetId inSet targetIds).filter(_.targetType === targetType).sortBy(_.id.desc) join
      accounts.map(t => (t.login, t.id, t.userType, t.name)) on { case (like, userProp) => like.ownerId === userProp._2 }).take(targetIds.length * maxLikesView)
      .result).map(_.map {
      case (like, userProp) =>
        val likeModel = likeFrom(like)
        likeModel.userLoginOpt = Some(userProp._1)
        likeModel.displayNameOpt = userProp._3 match {
          case AccountType.COMPANY => userProp._4
          case _                   => Some(userProp._1)
        }
        likeModel
    })
  }

  def findProductTitle(targetId: Long): Future[(Long, String)] =
    db.run(posts.filter(_.id === targetId).map(t => (t.id, t.title)).result.head)

  def findProductsTitles(targetIds: Seq[Long]): Future[Seq[(Long, String)]] =
    db.run((posts.filter(_.id inSet targetIds).map(t => (t.id, t.title)).result))

  def findTagsByTargetIds(targetType: Int, postIds: Seq[Long]): Future[Seq[(models.Tag, Long)]] = {
    val query = for {
      (tInp, t) <- tagsToTargets.filter(t => t.targetId.inSet(postIds) && t.targetType === targetType) join tags on { case (tInps, tg) => tInps.tagId === tg.id }
    } yield (tInp, t)
    db.run(query.result).map(_.map {
      case (tInp, t) => (tagFrom(t), tInp.targetId)
    })
  }

  def findTagsByTargetId(targetType: Int, targetId: Long): Future[Seq[models.Tag]] = {
    val query = for {
      (tInp, t) <- tagsToTargets.filter(t => t.targetId === targetId && t.targetType === targetType) join tags on { case (tInps, tg) => tInps.tagId === tg.id }
    } yield t
    db.run(query.result).map(_.map(tagFrom))
  }

  def findProductWithAccountByProductId(productId: Long, actorId: Long): Future[Option[models.Post]] = {
    val query = for {
      ((dbProduct, dbAccount), dbLikedOpt) <- posts.filter(t => t.id === productId && t.postType === PostType.PRODUCT) join
        accounts on { case (product, user) => product.ownerId === user.id } joinLeft
        likes.filter(t => t.targetType === TargetType.POST && t.ownerId === actorId) on { case ((product, user), like) => like.targetId === product.id }
    } yield (dbProduct, dbAccount, dbLikedOpt)
    db.run(query.result.headOption) map (_.map {
      case (dbProduct, dbAccount, dbLikedOpt) =>
        val product = postFrom(dbProduct)
        product.ownerOpt = Some(accountFrom(dbAccount))
        product.likedOpt = if (dbLikedOpt.isEmpty) Some(false) else Some(true)
        product
    }) flatMap (
      _ match {
        case Some(product) => findLikes(Seq(product.id), TargetType.POST).map { likes =>
          product.likes = likes
          Some(product)
        }
        case _ => Future.successful(None)
      }) /*flatMap (
        _ match {
          case Some(product) => findTagsByPostId(product.id).map { tags =>
            product.tags = tags
            Some(post)
          }
          case _ => Future.successful(None)
        })*/
  }

  def existsProductInCompany(productId: Long, accountId: Long) =
    db.run(posts.filter(t => t.id === productId && t.ownerId === accountId && t.postType === PostType.PRODUCT).exists.result)

  def enoughFunds(accountId: Long, currencyId: Int, funds: Long) =
    db.run(filterCurrentAccountBalance(accountId, currencyId).map(_.value >= funds).result.head)

  def findPostWithAccountByPostId(postId: Long, actorId: Long): Future[Option[models.Post]] = {
    val query = for {
      ((dbPost, dbAccount), dbLikedOpt) <- posts.filter(_.id === postId) join
        accounts on { case (post, user) => post.ownerId === user.id } joinLeft
        likes.filter(t => t.targetType === TargetType.POST && t.ownerId === actorId) on { case ((post, user), like) => like.targetId === post.id }
    } yield (dbPost, dbAccount, dbLikedOpt)
    db.run(query.result.headOption) map (_.map {
      case (dbPost, dbAccount, dbLikedOpt) =>
        val post = postFrom(dbPost)
        post.ownerOpt = Some(accountFrom(dbAccount))
        post.likedOpt = if (dbLikedOpt.isEmpty) Some(false) else Some(true)
        post
    }) flatMap (
      _ match {
        case Some(post) => findLikes(Seq(post.id), TargetType.POST).map { likes =>
          post.likes = likes
          post
        } flatMap (post => findProductsTitles(Seq(post).filter(_.productId.isDefined).map(_.productId.get)).map {
          case idTitleSeq =>
            Seq(post).filter(_.productId.isDefined).foreach(tpost => tpost.titleRefOpt = idTitleSeq.find(_._1 == tpost.productId.get).map(_._2))
            Some(post)
        })
        case _ => Future.successful(None)
      }) flatMap (
        _ match {
          case Some(post) => findTagsByTargetId(TargetType.POST, post.id).map { tags =>
            post.tags = tags
            Some(post)
          }
          case _ => Future.successful(None)
        })
  }

  def fillAccountBalances(account: models.Account): Future[Option[Account]] = {
    db.run {
      for {
        balanceTokens <- filterCurrentAccountBalanceValue(account.id, CurrencyType.TOKEN).result.headOption
        balanceDollars <- filterCurrentAccountBalanceValue(account.id, CurrencyType.DOLLAR).result.headOption
        balancePower <- filterCurrentAccountBalanceValue(account.id, CurrencyType.POWER).result.headOption
      } yield (balanceTokens, balanceDollars, balancePower)
    } map {
      case (balanceTokens, balanceDollars, balancePower) =>
        account.balanceTokenOpt = balanceTokens
        account.balanceDollarOpt = balanceDollars
        account.balancePowerOpt = balancePower
        Some(account)
    }
  }

  def findAccountBySessionKeyAndIPWithBalancesAndRoles(sessionKey: String, ip: String): Future[Option[models.Account]] = {
    val query = for {
      dbSession <- sessions.filter(t => t.sessionKey === sessionKey && t.ip === ip)
      dbAccount <- accounts.filter(_.id === dbSession.userId)
    } yield (dbAccount, dbSession)
    val result = db.run(query.result.headOption).map(_.map {
      case (dbAccount, dbSession) =>
        val user = accountFrom(dbAccount)
        user.sessionOpt = Some(sessionFrom(dbSession))
        user
    }).flatMap(_ match {
      case Some(account) => fillAccountBalances(account)
      case _             => Future.successful(None)
    })
    updateAccountWithRoles(result)
  }

  def approveScheduledTransactions(): Future[Int] = {
    val timestamp = System.currentTimeMillis

    val query = transactions.filter(t => t.state === TxState.SCHEDULED && t.scheduled < timestamp).result.flatMap { txs =>
      DBIO.sequence {
        txs.map { tx =>
          transactions.filter(_.id === tx.id)
            .map(itx => (itx.scheduled, itx.processed, itx.state))
            .update(None, Some(timestamp), TxState.APPROVED).zip {
              if (tx.toType == TargetType.ACCOUNT)
                updateCurrentAccountBalance(tx.toId.get, tx.currencyId, tx.amount)
              else
                DBIO.successful(0)
            }.map { case (a, b) => b }
        }
      }.map(_.sum)
    }
    db.run(query.transactionally)
  }

  def findPostsWithAccountsByText(pageId: Long, text: String, actorIdOpt: Option[Long]): Future[Seq[models.Post]] =
    actorIdOpt.fold(findPostsWithAccountsByText(pageId, text))(actorId => findPostsWithAccountsByText(pageId, text, actorId))

  def findPostsWithAccountsByText(pageId: Long, text: String, actorId: Long): Future[Seq[models.Post]] = {
    val searchString = "%" + text + "%"
    val query = for {
      (dbPost, dbLikedOpt) <- (posts.filter(t => t.moderateStatus === ModerateStatus.SUCCESS && (t.content.like(searchString) || t.title.like(searchString))) union
        posts.filter(t => t.moderateStatus === ModerateStatus.SUCCESS && (t.ownerId in {
          accounts.filter(_.login.like(searchString)).map(_.id)
        })))
        .joinLeft(likes.filter(t => t.targetType === TargetType.POST && t.ownerId === actorId)).on { case (post, like) => like.targetId === post.id }
        .sortBy(_._1.id.desc).drop(if (pageId > 0) pageSize * (pageId - 1) else 0).take(pageSize)
      dbAccount <- accounts.filter(_.id === dbPost.ownerId)
    } yield (dbPost, dbLikedOpt, dbAccount)
    db.run(query.result).map(_.map {
      case (dbPost, dbLikedOpt, dbAccount) =>
        val post = postFrom(dbPost)
        post.ownerOpt = Some(accountFrom(dbAccount))
        post.likedOpt = Some(if (dbLikedOpt.isDefined) true else false)
        post
    }) flatMap (posts =>
      findLikes(posts.map(_.id), TargetType.POST).map { likes =>
        posts.foreach(post => post.likes = likes.filter(_.targetId == post.id).take(maxLikesView))
        posts
      }) flatMap (posts =>
      findTagsByTargetIds(TargetType.POST, posts.map(_.id)).map { touples =>
        posts.foreach { post => post.tags = touples.filter(_._2 == post.id).map(_._1) }
        posts
      })
  }

  def findPostsWithAccountsByText(pageId: Long, text: String): Future[Seq[models.Post]] = {
    val searchString = "%" + text + "%"
    val query = for {
      dbPost <- (posts.filter(t => t.moderateStatus === ModerateStatus.SUCCESS && (t.content.like(searchString) || t.title.like(searchString))) union
        posts.filter(t => t.moderateStatus === ModerateStatus.SUCCESS && (t.ownerId in {
          accounts.filter(_.login.like(searchString)).map(_.id)
        }))).sortBy(_.id.desc).drop(if (pageId > 0) pageSize * (pageId - 1) else 0).take(pageSize)
      //dbLiked.joinLeft(likes.filter(t => t.targetType === TargetType.POST && t.ownerId === actorId)).on { case ((post, user), like) => like.targetId === post.id }
      dbAccount <- accounts.filter(_.id === dbPost.ownerId)
    } yield (dbPost, dbAccount)
    db.run(query.result).map(_.map {
      case (dbPost, dbAccount) =>
        val post = postFrom(dbPost)
        post.ownerOpt = Some(accountFrom(dbAccount))
        post
    }) flatMap (posts =>
      findProductsTitles(posts.filter(_.productId.isDefined).map(_.productId.get)).map {
        case idTitleSeq =>
          posts.filter(_.productId.isDefined).foreach(post => post.titleRefOpt = idTitleSeq.find(_._1 == post.productId.get).map(_._2))
          posts
      }) flatMap (posts =>
      findLikes(posts.map(_.id), TargetType.POST).map { likes =>
        posts.foreach(post => post.likes = likes.filter(_.targetId == post.id).take(maxLikesView))
        posts
      }) flatMap (posts =>
      findTagsByTargetIds(TargetType.POST, posts.map(_.id)).map { touples =>
        posts.foreach { post => post.tags = touples.filter(_._2 == post.id).map(_._1) }
        posts
      })
  }

  def findProductsPagesWithAccountsByCategoryTagNames(
    actorIdOpt:  Option[Long],
    userIdOpt:   Option[Long],
    categoryOpt: Option[String],
    tagNamesOpt: Option[Seq[String]])(implicit ac: AppContext): Future[Int] =
    tagNamesOpt match {
      case Some(tagNames) =>
        db.run(tags.filter(_.name inSet tagNames).map(_.id).result) flatMap { tagIds =>
          actorIdOpt.fold(findProductsPagesWithAccountsByCategoryTagIds(userIdOpt, categoryOpt, Some(tagIds)))(actorId =>
            findProductsPagesWithAccountsByCategoryTagIds(actorId, userIdOpt, categoryOpt, Some(tagIds)))
        }
      case _ =>
        actorIdOpt.fold(findProductsPagesWithAccountsByCategoryTagIds(userIdOpt, categoryOpt, None))(actorId =>
          findProductsPagesWithAccountsByCategoryTagIds(actorId, userIdOpt, categoryOpt, None))
    }

  def findProductsWithAccountsByCategoryTagNames(
    actorIdOpt:  Option[Long],
    userIdOpt:   Option[Long],
    categoryOpt: Option[String],
    pageId:      Long, tagNamesOpt: Option[Seq[String]])(implicit ac: AppContext): Future[Seq[models.Post]] =
    tagNamesOpt match {
      case Some(tagNames) =>
        db.run(tags.filter(_.name inSet tagNames).map(_.id).result) flatMap { tagIds =>
          actorIdOpt.fold(findProductsWithAccountsByCategoryTagIds(userIdOpt, categoryOpt, pageId, Some(tagIds)))(actorId =>
            findProductsWithAccountsByCategoryTagIds(actorId, userIdOpt, categoryOpt, pageId, Some(tagIds)))
        }
      case _ =>
        actorIdOpt.fold(findProductsWithAccountsByCategoryTagIds(userIdOpt, categoryOpt, pageId, None))(actorId =>
          findProductsWithAccountsByCategoryTagIds(actorId, userIdOpt, categoryOpt, pageId, None))
    }

  def findProductsWithAccountsByCategoryTagIdsQuery(
    userIdOpt:   Option[Long],
    categoryOpt: Option[String],
    tagIdsOpt:   Option[Seq[Long]])(implicit ac: AppContext) = {
    ({
      val first = userIdOpt match {
        case Some(userId) => posts.filter(t => t.moderateStatus === ModerateStatus.SUCCESS && t.ownerId === userId && t.postType === PostType.PRODUCT)
        case _            => posts
      }
      tagIdsOpt match {
        case Some(tagIds) => first filter (_.id in {
          tagsToTargets.filter(t => t.tagId.inSet(tagIds) && t.targetType === TargetType.POST).map(_.targetId)
        })
        case _ => first
      }
    }
      join accounts on { case (post, user) => post.ownerId === user.id })
      .sortBy {
        case (post, user) =>
          (categoryOpt match {
            case Some(PostsFilter.NEW)      => post.created.desc
            case Some(PostsFilter.HOT)      => post.commentsCount.desc
            case Some(PostsFilter.TRENDING) => post.likesCount.desc
            case Some(PostsFilter.REVIEWS)  => post.reviewsCount.desc
            case _                          => post.id.desc
          })
      }
  }

  def findProductsPagesWithAccountsByCategoryTagIds(
    userIdOpt:   Option[Long],
    categoryOpt: Option[String],
    tagIdsOpt:   Option[Seq[Long]])(implicit ac: AppContext): Future[Int] = {
    db.run(findProductsWithAccountsByCategoryTagIdsQuery(
      userIdOpt,
      categoryOpt,
      tagIdsOpt).size.result) map pages
  }

  def findProductsWithAccountsByCategoryTagIds(
    userIdOpt:   Option[Long],
    categoryOpt: Option[String],
    pageId:      Long,
    tagIdsOpt:   Option[Seq[Long]])(implicit ac: AppContext): Future[Seq[models.Post]] = {
    val query = for {
      (dbProduct, dbAccount) <- findProductsWithAccountsByCategoryTagIdsQuery(
        userIdOpt,
        categoryOpt,
        tagIdsOpt).drop(if (pageId > 0) pageSize * (pageId - 1) else 0).take(pageSize)
    } yield (dbProduct, dbAccount)
    db.run(query.result).map(_.map {
      case (dbProduct, dbAccount) =>
        val product = postFrom(dbProduct)
        product.ownerOpt = Some(accountFrom(dbAccount))
        product
    }) flatMap (products =>
      findLikes(products.map(_.id), TargetType.PRODUCT).map { likes =>
        products.foreach(product => product.likes = likes.filter(_.targetId == product.id).take(maxLikesView))
        products
      }) /*flatMap (products =>
      findTagsByPostIds(products.map(_.id)).map { touples =>
        products.foreach { product => product.tags = touples.filter(_._2 == product.id).map(_._1) }
        products
      })*/
  }

  def findReviewsWithAccountsByCategoryTagNames(
    actorIdOpt: Option[Long],
    productId:  Long,
    pageId:     Long)(implicit ac: AppContext): Future[Seq[models.Post]] =
    actorIdOpt.fold(findReviewsWithAccountsByCategoryTagIds(productId, pageId))(actorId =>
      findReviewsWithAccountsByCategoryTagIds(actorId, productId, pageId))

  def findReviewsPagesWithAccountsByCategoryTagNames(
    productId: Long)(implicit ac: AppContext): Future[Int] =
    findReviewsPagesWithAccountsByCategoryTagIds(productId)

  def findReviewsPagesWithAccountsByCategoryTagIds(
    productId: Long)(implicit ac: AppContext): Future[Int] = {
    val pageSize = 5
    db.run(posts.filter(t => t.moderateStatus === ModerateStatus.SUCCESS && t.productId === productId && t.postType === PostType.REVIEW).size.result) map pages
  }

  def findReviewsWithAccountsByCategoryTagIds(
    productId: Long,
    pageId:    Long)(implicit ac: AppContext): Future[Seq[models.Post]] = {
    val pageSize = 5
    val query =
      (posts.filter(t => t.moderateStatus === ModerateStatus.SUCCESS && t.productId === productId && t.postType === PostType.REVIEW)
        join accounts on { case (post, user) => post.ownerId === user.id })
        .sortBy {
          case (post, account) => post.id.desc
        }.drop(if (pageId > 0) pageSize * (pageId - 1) else 0).take(pageSize)
    db.run(query.result).map(_.map {
      case (dbReview, dbAccount) =>
        val product = postFrom(dbReview)
        product.ownerOpt = Some(accountFrom(dbAccount))
        product
    }) flatMap (reviews =>
      findProductsTitles(reviews.filter(_.productId.isDefined).map(_.productId.get)).map {
        case idTitleSeq =>
          reviews.filter(_.productId.isDefined).foreach(post => post.titleRefOpt = idTitleSeq.find(_._1 == post.productId.get).map(_._2))
          reviews
      }) flatMap (reviews =>
      findLikes(reviews.map(_.id), TargetType.POST).map { likes =>
        reviews.foreach(review => review.likes = likes.filter(_.targetId == review.id).take(maxLikesView))
        reviews
      })
  }

  def findReviewsWithAccountsByCategoryTagIds(
    actorId:   Long,
    productId: Long,
    pageId:    Long)(implicit ac: AppContext): Future[Seq[models.Post]] = {
    val pageSize = 5
    val query =
      (posts.filter(t => t.moderateStatus === ModerateStatus.SUCCESS && t.productId === productId && t.postType === PostType.REVIEW)
        join accounts on { case (post, user) => post.ownerId === user.id })
        .joinLeft(likes.filter(t => t.targetType === TargetType.POST && t.ownerId === actorId)).on { case ((post, user), like) => like.targetId === post.id }
        .sortBy {
          case ((post, account), likeOpt) => post.id.desc
        }.drop(if (pageId > 0) pageSize * (pageId - 1) else 0).take(pageSize)
    db.run(query.result).map(_.map {
      case ((dbReview, dbAccount), dbLikeOpt) =>
        val review = postFrom(dbReview)
        review.ownerOpt = Some(accountFrom(dbAccount))
        review.likedOpt = if (dbLikeOpt.isEmpty) Some(false) else Some(true)
        review
    }) flatMap (reviews =>
      findProductsTitles(reviews.filter(_.productId.isDefined).map(_.productId.get)).map {
        case idTitleSeq =>
          reviews.filter(_.productId.isDefined).foreach(post => post.titleRefOpt = idTitleSeq.find(_._1 == post.productId.get).map(_._2))
          reviews
      }) flatMap (reviews =>
      findLikes(reviews.map(_.id), TargetType.POST).map { likes =>
        reviews.foreach(review => review.likes = likes.filter(_.targetId == review.id).take(maxLikesView))
        reviews
      })
  }

  def findPostsWithAccountsByCategoryTagNames(
    actorIdOpt:       Option[Long],
    userIdOpt:        Option[Long],
    categoryOpt:      Option[String],
    pageId:           Long,
    tagNamesOpt:      Option[Seq[String]],
    withNotModerated: Boolean): Future[Seq[models.Post]] =
    tagNamesOpt match {
      case Some(tagNames) =>
        db.run(tags.filter(_.name inSet tagNames).map(_.id).result) flatMap { tagIds =>
          actorIdOpt.fold(findPostsWithAccountsByCategoryTagIds(userIdOpt, categoryOpt, pageId, Some(tagIds), withNotModerated))(actorId =>
            findPostsWithAccountsByCategoryTagIds(actorId, userIdOpt, categoryOpt, pageId, Some(tagIds), withNotModerated))
        }
      case _ =>
        actorIdOpt.fold(findPostsWithAccountsByCategoryTagIds(userIdOpt, categoryOpt, pageId, None, withNotModerated))(actorId =>
          findPostsWithAccountsByCategoryTagIds(actorId, userIdOpt, categoryOpt, pageId, None, withNotModerated))
    }

  def findPostsPagesWithAccountsByCategoryTagNames(
    userIdOpt:        Option[Long],
    categoryOpt:      Option[String],
    tagNamesOpt:      Option[Seq[String]],
    withNotModerated: Boolean): Future[Int] =
    tagNamesOpt match {
      case Some(tagNames) =>
        db.run(tags.filter(_.name inSet tagNames).map(_.id).result) flatMap { tagIds =>
          findPostsPagesWithAccountsByCategoryTagIds(userIdOpt, categoryOpt, Some(tagIds), withNotModerated)
        }
      case _ =>
        findPostsPagesWithAccountsByCategoryTagIds(userIdOpt, categoryOpt, None, withNotModerated)
    }

  def findPostsPagesWithAccountsByCategoryTagIds(
    userIdOpt:        Option[Long],
    categoryOpt:      Option[String],
    tagIdsOpt:        Option[Seq[Long]],
    withNotModerated: Boolean) = {
    db.run(findPostsWithAccountsByCategoryTagIdsQuery(userIdOpt, categoryOpt, tagIdsOpt, withNotModerated).size.result) map pages
  }

  def findPostsWithAccountsByCategoryTagIdsQuery(
    userIdOpt:        Option[Long],
    categoryOpt:      Option[String],
    tagIdsOpt:        Option[Seq[Long]],
    withNotModerated: Boolean) = {

    val first = userIdOpt match {
      case Some(userId) => posts.filter(_.ownerId === userId)
      case _            => posts
    }

    val second = (userIdOpt, withNotModerated) match {
      case (_, false) => first.filter(_.moderateStatus === ModerateStatus.SUCCESS)
      case _          => first
    }

    tagIdsOpt match {
      case Some(tagIds) => second filter (_.id in {
        tagsToTargets.filter(t => t.tagId.inSet(tagIds) && t.targetType === TargetType.POST).map(_.targetId)
      })
      case _ => second
    }
  }

  def findPostsWithAccountsByCategoryTagIds(
    userIdOpt:        Option[Long],
    categoryOpt:      Option[String],
    pageId:           Long,
    tagIdsOpt:        Option[Seq[Long]],
    withNotModerated: Boolean): Future[Seq[models.Post]] = {
    val query = for {
      (dbPost, dbAccount) <- (findPostsWithAccountsByCategoryTagIdsQuery(userIdOpt, categoryOpt, tagIdsOpt, withNotModerated)
        join accounts on { case (post, user) => post.ownerId === user.id })
        .sortBy {
          case (post, user) =>
            (categoryOpt match {
              case Some(PostsFilter.NEW)       => post.created.desc
              case Some(PostsFilter.HOT)       => post.commentsCount.desc
              case Some(PostsFilter.TRENDING)  => post.likesCount.desc
              case Some(PostsFilter.PROMOUTED) => post.promo.desc
              case _                           => post.id.desc
            })
        }.drop(if (pageId > 0) pageSize * (pageId - 1) else 0).take(pageSize)
    } yield (dbPost, dbAccount)
    db.run(query.result).map(_.map {
      case (dbPost, dbAccount) =>
        val post = postFrom(dbPost)
        post.ownerOpt = Some(accountFrom(dbAccount))
        post
    }) flatMap (posts =>
      findProductsTitles(posts.filter(_.productId.isDefined).map(_.productId.get)).map {
        case idTitleSeq =>
          posts.filter(_.productId.isDefined).foreach(post => post.titleRefOpt = idTitleSeq.find(_._1 == post.productId.get).map(_._2))
          posts
      }) flatMap (posts =>
      findLikes(posts.map(_.id), TargetType.POST).map { likes =>
        posts.foreach(post => post.likes = likes.filter(_.targetId == post.id).take(maxLikesView))
        posts
      }) flatMap (posts =>
      findTagsByTargetIds(TargetType.POST, posts.map(_.id)).map { touples =>
        posts.foreach { post => post.tags = touples.filter(_._2 == post.id).map(_._1) }
        posts
      })
  }

  def findPostsWithAccountsByCategoryTagIds(
    actorId:          Long,
    userIdOpt:        Option[Long],
    categoryOpt:      Option[String],
    pageId:           Long,
    tagIdsOpt:        Option[Seq[Long]],
    withNotModerated: Boolean): Future[Seq[models.Post]] = {
    val query = for {
      (dbPost, dbAccount) <- (findPostsWithAccountsByCategoryTagIdsQuery(userIdOpt, categoryOpt, tagIdsOpt, withNotModerated)
        join accounts on { case (post, user) => post.ownerId === user.id })
        //.joinLeft(likes.filter(t => t.targetType === TargetType.POST && t.ownerId === actorId)).on { case ((post, user), like) => like.targetId === post.id }
        .sortBy {
          case (post, user) =>
            (categoryOpt match {
              case Some(PostsFilter.NEW)       => post.created.desc
              case Some(PostsFilter.HOT)       => post.commentsCount.desc
              case Some(PostsFilter.TRENDING)  => post.likesCount.desc
              case Some(PostsFilter.PROMOUTED) => post.promo.desc
              case _                           => post.id.desc
            })
        }.drop(if (pageId > 0) pageSize * (pageId - 1) else 0).take(pageSize)
    } yield (dbPost, dbAccount)
    db.run(query.result).map(_.map {
      case (dbPost, dbAccount) =>
        val post = postFrom(dbPost)
        post.ownerOpt = Some(accountFrom(dbAccount))
        //post.likedOpt = if (dbLikeOpt.isEmpty) Some(false) else Some(true)
        post
    }) flatMap (posts =>
      findProductsTitles(posts.filter(_.productId.isDefined).map(_.productId.get)).map {
        case idTitleSeq =>
          posts.filter(_.productId.isDefined).foreach(post => post.titleRefOpt = idTitleSeq.find(_._1 == post.productId.get).map(_._2))
          posts
      }) flatMap (posts =>
      findLikes(posts.map(_.id), TargetType.POST).map { likes =>
        posts.foreach(post => post.likes = likes.filter(_.targetId == post.id).take(maxLikesView))
        posts
      }) flatMap (posts =>
      findTagsByTargetIds(TargetType.POST, posts.map(_.id)).map { touples =>
        posts.foreach { post => post.tags = touples.filter(_._2 == post.id).map(_._1) }
        posts
      }) flatMap { posts =>
      db.run(likes
        .filter(t => t.targetType === TargetType.POST && t.ownerId === actorId && t.targetId.inSet(posts.map(_.id)))
        .map(_.targetId)
        .result)
        .map { likeIds =>
          posts.foreach(post => post.likedOpt = Some(likeIds.exists(_ == post.id)))
          posts
        }
    }
  }

  def findProductsWithAccountsByCategoryTagIdsQuery(
    actorId:     Long,
    userIdOpt:   Option[Long],
    categoryOpt: Option[String],
    tagIdsOpt:   Option[Seq[Long]])(implicit ac: AppContext) = {
    ({
      val first = userIdOpt match {
        case Some(userId) => posts.filter(t => t.moderateStatus === ModerateStatus.SUCCESS && t.ownerId === userId && t.postType === PostType.PRODUCT)
        case _            => posts
      }
      tagIdsOpt match {
        case Some(tagIds) => first filter (_.id in {
          tagsToTargets.filter(t => t.tagId.inSet(tagIds) && t.targetType === TargetType.POST).map(_.targetId)
        })
        case _ => first
      }
    }
      join accounts on { case (post, user) => post.ownerId === user.id })
      .joinLeft(likes.filter(t => t.targetType === TargetType.POST && t.ownerId === actorId)).on { case ((product, user), like) => like.targetId === product.id }
      .sortBy {
        case ((post, user), like) =>
          (categoryOpt match {
            case Some(PostsFilter.NEW)      => post.created.desc
            case Some(PostsFilter.HOT)      => post.commentsCount.desc
            case Some(PostsFilter.TRENDING) => post.likesCount.desc
            case Some(PostsFilter.REVIEWS)  => post.reviewsCount.desc
            case _                          => post.id.desc
          })
      }
  }

  def findProductsWithAccountsByCategoryTagIds(
    actorId:     Long,
    userIdOpt:   Option[Long],
    categoryOpt: Option[String],
    pageId:      Long,
    tagIdsOpt:   Option[Seq[Long]])(implicit ac: AppContext): Future[Seq[models.Post]] = {
    val query = for {
      ((dbProduct, dbAccount), dbLikeOpt) <- findProductsWithAccountsByCategoryTagIdsQuery(
        actorId,
        userIdOpt,
        categoryOpt,
        tagIdsOpt).drop(if (pageId > 0) pageSize * (pageId - 1) else 0).take(pageSize)
    } yield (dbProduct, dbAccount, dbLikeOpt)
    db.run(query.result).map(_.map {
      case (dbProduct, dbAccount, dbLikeOpt) =>
        val product = postFrom(dbProduct)
        product.ownerOpt = Some(accountFrom(dbAccount))
        product.likedOpt = if (dbLikeOpt.isEmpty) Some(false) else Some(true)
        product
    }) flatMap (products =>
      findLikes(products.map(_.id), TargetType.POST).map { likes =>
        products.foreach(product => product.likes = likes.filter(_.targetId == product.id).take(maxLikesView))
        products
      }) /*flatMap (posts =>
      findTagsByPostIds(products.map(_.id)).map { touples =>
        products.foreach { product => product.tags = touples.filter(_._2 == product.id).map(_._1) }
        products
      })*/
  }

  def findProductsPagesWithAccountsByCategoryTagIds(
    actorId:     Long,
    userIdOpt:   Option[Long],
    categoryOpt: Option[String],
    tagIdsOpt:   Option[Seq[Long]])(implicit ac: AppContext): Future[Int] = {
    db.run(findProductsWithAccountsByCategoryTagIdsQuery(
      actorId,
      userIdOpt,
      categoryOpt,
      tagIdsOpt).size.result) map pages
  }

  def findProductsWithAccountsByCategoryByNames(
    actorIdOpt:  Option[Long],
    userLogin:   String,
    categoryOpt: Option[String],
    pageId:      Long,
    tagNamesOpt: Option[Seq[String]])(implicit ac: AppContext): Future[Seq[models.Post]] =
    db.run(accounts.filter(_.login === userLogin).result.headOption).flatMap(_.fold(Future(Seq[models.Post]())) { user =>
      findProductsWithAccountsByCategoryTagNames(actorIdOpt, Some(user.id), categoryOpt, pageId, tagNamesOpt)
    })

  def findProductsPagesWithAccountsByCategoryByNames(
    actorIdOpt:  Option[Long],
    userLogin:   String,
    categoryOpt: Option[String],
    tagNamesOpt: Option[Seq[String]])(implicit ac: AppContext): Future[Int] =
    db.run(accounts.filter(_.login === userLogin).result.headOption).flatMap(_.fold(Future(0)) { user =>
      findProductsPagesWithAccountsByCategoryTagNames(actorIdOpt, Some(user.id), categoryOpt, tagNamesOpt)
    })

  def findPostsWithAccountsByCategoryByNames(
    actorIdOpt:       Option[Long],
    userLogin:        String,
    categoryOpt:      Option[String],
    pageId:           Long,
    tagNamesOpt:      Option[Seq[String]],
    withNotModerated: Boolean)(implicit ac: AppContext): Future[Seq[models.Post]] =
    db.run(accounts.filter(_.login === userLogin).result.headOption).flatMap(_.fold(Future(Seq[models.Post]())) { user =>
      findPostsWithAccountsByCategoryTagNames(actorIdOpt, Some(user.id), categoryOpt, pageId, tagNamesOpt, withNotModerated)
    })

  def findAccountBySUIDAndSessionId(sessionId: Long, sessionKey: String): Future[Option[Account]] = {
    val query = for {
      dbSession <- sessions.filter(t => t.id === sessionId && t.sessionKey === sessionKey)
      dbAccount <- accounts.filter(_.id === dbSession.userId)
    } yield (dbAccount, dbSession)
    db.run(query.result.headOption).map(_.map {
      case (dbAccount, dbSession) =>
        val user = accountFrom(dbAccount)
        user.sessionOpt = Some(sessionFrom(dbSession))
        user
    })
  }

  def findCommentsWithAccountsByPostId(postId: Long, userIdOpt: Option[Long]): Future[Seq[models.Comment]] =
    userIdOpt.fold(findCommentsWithAccountsByPostId(postId))(t => findCommentsWithAccountsByPostId(postId, t))

  def findCommentsWithAccountsByPostId(postId: Long, userId: Long): Future[Seq[models.Comment]] = {
    val query = for {
      ((dbComment, dbAccount), dbLikeOpt) <- comments.filter(_.postId === postId) join
        accounts on (_.ownerId === _.id) joinLeft
        likes.filter(t => t.targetType === TargetType.COMMENT && t.ownerId === userId) on { case ((c, u), l) => c.id === l.targetId }
    } yield (dbComment, dbAccount, dbLikeOpt)
    db.run(query.result).map(_.map {
      case (dbComment, dbAccount, dbLikeOpt) =>
        val comment = commentFrom(dbComment)
        comment.ownerOpt = Some(accountFrom(dbAccount))
        comment.likedOpt = if (dbLikeOpt.isEmpty) Some(false) else Some(true)
        comment
    }) flatMap (comments =>
      findLikes(comments.map(_.id), TargetType.COMMENT).map { likes =>
        comments.foreach(comment => comment.likes = likes.filter(_.targetId == comment.id).take(maxLikesView))
        comments
      })
  }

  def findCommentsWithAccountsByPostId(postId: Long): Future[Seq[models.Comment]] = {
    val query = for {
      dbComment <- comments.filter(_.postId === postId)
      dbAccount <- accounts.filter(_.id === dbComment.ownerId)
    } yield (dbComment, dbAccount)
    db.run(query.result).map(_.map {
      case (dbComment, dbAccount) =>
        val comment = commentFrom(dbComment)
        comment.ownerOpt = Some(accountFrom(dbAccount))
        comment
    }) flatMap (comments =>
      findLikes(comments.map(_.id), TargetType.COMMENT).map { likes =>
        comments.foreach(comment => comment.likes = likes.filter(_.targetId == comment.id))
        comments
      })
  }

  def findCommentsWithAccountsForAllAccountPosts(login: String, pageId: Long, userIdOpt: Option[Long]): Future[Seq[models.Comment]] =
    db.run(accounts.filter(_.login === login).result.headOption) flatMap (_.fold(Future(Seq[models.Comment]())) { user =>
      findCommentsWithAccountsForAllAccountPosts(user.id, pageId, userIdOpt)
    })

  def findCommentsWithAccountsForAllAccountPosts(userId: Long, pageId: Long, userIdOpt: Option[Long]): Future[Seq[models.Comment]] =
    userIdOpt.fold(findCommentsWithAccountsForAllAccountPosts(userId, pageId))(uid => findCommentsWithAccountsForAllAccountPosts(userId, pageId, uid))

  def findCommentsWithAccountsForAllAccountPosts(userId: Long, pageId: Long, actorAccountId: Long): Future[Seq[models.Comment]] = {
    val query = for {
      (((dbPost, dbComment), dbAccount), dbLikeOpt) <- (posts.filter(_.ownerId === userId) join
        comments on (_.id === _.postId) join
        accounts on { case ((post, comment), user) => user.id === comment.ownerId } joinLeft
        likes.filter(t => t.targetType === TargetType.COMMENT && t.ownerId === actorAccountId) on { case (((post, comment), user), like) => like.targetId === comment.id })
        .sortBy { case (((post, comment), user), likeOpt) => comment.id.desc }.drop(if (pageId > 0) pageSize * (pageId - 1) else 0).take(pageSize)
    } yield (dbComment, dbAccount, dbLikeOpt)
    db.run(query.result).map(_.map {
      case (dbComment, dbAccount, dbLikeOpt) =>
        val comment = commentFrom(dbComment)
        comment.ownerOpt = Some(accountFrom(dbAccount))
        comment.likedOpt = if (dbLikeOpt.isEmpty) Some(false) else Some(true)
        comment
    }) flatMap (comments =>
      findLikes(comments.map(_.id), TargetType.COMMENT).map { likes =>
        comments.foreach(comment => comment.likes = likes.filter(_.targetId == comment.id).take(maxLikesView))
        comments
      })
  }

  def findCommentsPagesWithAccountsForAllAccountPosts(userId: Long): Future[Int] = {
    db.run(posts.filter(_.ownerId === userId).join(comments).on(_.id === _.postId).size.result) map pages
  }

  def findCommentsWithAccountsForAllAccountPosts(userId: Long, pageId: Long): Future[Seq[models.Comment]] = {
    val query = for {
      ((dbPost, dbComment), dbAccount) <- (posts.filter(_.ownerId === userId) join
        comments on (_.id === _.postId) join
        accounts on { case ((post, comment), user) => user.id === comment.ownerId })
        .sortBy { case ((post, comment), user) => comment.id.desc }.drop(if (pageId > 0) pageSize * (pageId - 1) else 0).take(pageSize)
    } yield (dbComment, dbAccount)
    db.run(query.result).map(_.map {
      case (dbComment, dbAccount) =>
        val comment = commentFrom(dbComment)
        comment.ownerOpt = Some(accountFrom(dbAccount))
        comment
    }) flatMap (comments =>
      findLikes(comments.map(_.id), TargetType.COMMENT).map { likes =>
        comments.foreach(comment => comment.likes = likes.filter(_.targetId == comment.id).take(maxLikesView))
        comments
      })
  }

  def isAlreadyLikeCommentByAccount(userId: Long, commentId: Long): Future[Boolean] =
    db.run(likes
      .filter(t => t.ownerId === userId && t.targetId === commentId && t.targetType === TargetType.COMMENT)
      .result
      .headOption).map(_.fold(false)(_ => true))

  def isAlreadyLikePostByAccount(userId: Long, postId: Long): Future[Boolean] =
    db.run(likes
      .filter(t => t.ownerId === userId && t.targetId === postId && t.targetType === TargetType.POST)
      .result
      .headOption).map(_.fold(false)(_ => true))

  def isProductExistsById(productId: Long): Future[Boolean] =
    db.run(posts.filter(t => t.id === productId && t.postType === PostType.PRODUCT).exists.result)

  def findCommentById(id: Long): Future[Option[models.Comment]] =
    getCommentFromQuery(comments.filter(_.id === id))

  def getCommentFromQuery(query: Query[(Comments), (DBComment), Seq]): Future[Option[models.Comment]] =
    db.run(query.result.headOption).map(_.map(commentFrom))

  def findPostById(id: Long): Future[Option[models.Post]] =
    getPostFromQuery(posts.filter(_.id === id))

  def getPostFromQuery(query: Query[(Posts), (DBPost), Seq]): Future[Option[models.Post]] =
    db.run(query.result.headOption).map(_.map(postFrom))

  def getAccountFromQuery(query: Query[(Accounts), (DBAccount), Seq]): Future[Option[models.Account]] =
    db.run(query.result.headOption).map(_.map(accountFrom))

  def getSessionFromQuery(query: Query[(Sessions), (DBSession), Seq]): Future[Option[models.Session]] =
    db.run(query.result.headOption).map(_.map(sessionFrom))

  def createPostWithPostsCounterUpdate(
    userId:         Long,
    productIdOpt:   Option[Long],
    title:          String,
    content:        String,
    thumbnail:      Option[String],
    rewardType:     Int,
    postType:       Int,
    limit:          Option[Long],
    tagNames:       Seq[String],
    moderateOption: Boolean): Future[Option[models.Post]] = {

    val query = for {
      user <- accounts.filter(_.id === userId).result.head
      post <- (posts returning posts.map(_.id) into ((v, id) => v.copy(id = id))) += new models.daos.DBPost(
        0,
        userId,
        productIdOpt,
        title,
        thumbnail,
        content,
        models.ContentType.MARKDOWN,
        postType,
        Some(0),
        limit,
        models.PostStatus.ACTIVE,
        0,
        None,
        None,
        None,
        models.PostStatus.ACTIVE,
        0,
        0,
        0,
        System.currentTimeMillis,
        0,
        rewardType,
        0,
        0,
        0,
        0,
        0,
        if (moderateOption) ModerateStatus.NEED_A_MODERATION else ModerateStatus.SUCCESS)
      productReviewsCountOpt <- productIdOpt match {
        case Some(productId) =>
          posts.filter(t => t.id === productId && t.postType === PostType.PRODUCT)
            .map(_.reviewsCount).result.headOption
        case _ => DBIO.successful(None)
      }
      _ <- productReviewsCountOpt match {
        case Some(productReviewsCount) =>
          posts.filter(t => t.id === productIdOpt.get && t.postType === PostType.PRODUCT)
            .map(_.reviewsCount).update(productReviewsCount + 1)
        case _ => DBIO.successful(None)
      }
      _ <- accounts.filter(_.id === userId)
        .map(t => (t.postsCount, t.postsCounter))
        .update(user.postsCount + 1, user.postsCounter + 1)
      actualPost <- posts.filter(_.id === post.id).result.head
    } yield actualPost

    db.run(query.transactionally) flatMap { dbPost =>
      getOrCreateTags(tagNames) flatMap { tags =>
        assignTagsToPost(tags.map(_.id), dbPost.id).map { _ =>
          val post = postFrom(dbPost)
          post.tags = tags
          Some(post)
        }
      }
    }
  }

  def createProductWithPostsCounterUpdate(
    userId:         Long,
    name:           String,
    about:          String,
    thumbnail:      Option[String],
    addressOpt:     Option[String],
    value:          Int,
    alcohol:        Int,
    tagNames:       Seq[String],
    moderateOption: Boolean): Future[Option[models.Post]] = {

    val query = for {
      user <- accounts.filter(_.id === userId).result.head
      product <- (posts returning posts.map(_.id) into ((v, id) => v.copy(id = id))) += new models.daos.DBPost(
        0,
        userId,
        None,
        name,
        thumbnail,
        about,
        models.ContentType.MARKDOWN,
        PostType.PRODUCT,
        Some(0),
        Some(0),
        models.PostStatus.ACTIVE,
        0,
        Some(alcohol),
        Some(value),
        addressOpt,
        models.PostStatus.ACTIVE,
        0,
        0,
        0,
        System.currentTimeMillis,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        if (moderateOption) ModerateStatus.NEED_A_MODERATION else ModerateStatus.SUCCESS)
      _ <- accounts.filter(_.id === userId)
        .map(t => (t.postsCount, t.postsCounter))
        .update(user.postsCount + 1, user.postsCounter + 1)
    } yield product

    db.run(query.transactionally) map (t => Some(postFrom(t)))
    //    db.run(query.transactionally) flatMap { dbProduct =>
    //      getOrCreateTags(tagNames) flatMap { tags =>
    //        assignTagsToPost(tags.map(_.id), dbProduct.id).map { _ =>
    //          val post = productFrom(dbProduct)
    //          post.tags = tags
    //          Some(post)
    //        }
    //      }
    //    }
  }

  def assignTagsToPost(tagsIds: Seq[Long], postId: Long) =
    db.run(DBIO.sequence(tagsIds.map(tagId => tagsToTargets += DBTagToTarget(tagId, postId, TargetType.POST, System.currentTimeMillis))).map(_.sum).transactionally)

  def getOrCreateTags(names: Seq[String]): Future[Seq[models.Tag]] =
    db.run(DBIO.sequence(names.map(getOrCreateTagAction)).transactionally).map(_.map(tagFrom))

  def getOrCreateTagAction(name: String) =
    tags.filter(_.name === name).result.headOption.flatMap {
      case Some(tag) => DBIO.successful(tag)
      case None      => (tags returning tags.map(_.id) into ((v, id) => v.copy(id = id))) += DBTag(0, name)
    }

  def currencySelectorCommentRewardDAO(currency: Int, t: DAO.this.Comments) =
    currency match {
      case CurrencyType.TOKEN  => t.rewardToken
      case CurrencyType.POWER  => t.rewardPower
      case CurrencyType.DOLLAR => t.rewardDollar
    }

  def currencySelectorCommentRewardDB(currency: Int, t: DBComment) =
    currency match {
      case CurrencyType.TOKEN  => t.rewardToken
      case CurrencyType.POWER  => t.rewardPower
      case CurrencyType.DOLLAR => t.rewardDollar
    }

  def currencySelectorPostRewardDAO(currency: Int, t: DAO.this.Posts) =
    currency match {
      case CurrencyType.TOKEN  => t.rewardToken
      case CurrencyType.POWER  => t.rewardPower
      case CurrencyType.DOLLAR => t.rewardDollar
    }

  def currencySelectorPostRewardDB(currency: Int, t: DBPost) =
    currency match {
      case CurrencyType.TOKEN  => t.rewardToken
      case CurrencyType.POWER  => t.rewardPower
      case CurrencyType.DOLLAR => t.rewardDollar
    }

  def udateCommentReward(optTx: Option[DBTransaction], comment: DBComment, reward: Long, currency: Int): DBIOAction[Int, NoStream, Effect.Write] =
    optTx match {
      case Some(tx) => comments.filter(_.id === comment.id)
        .map(t => currencySelectorCommentRewardDAO(currency, t))
        .update(currencySelectorCommentRewardDB(currency, comment) + reward)
      case _ => DBIO.successful(0)
    }

  def udatePostReward(optTx: Option[DBTransaction], post: DBPost, reward: Long, currency: Int): DBIOAction[Int, NoStream, Effect.Write] =
    optTx match {
      case Some(tx) => posts.filter(_.id === post.id)
        .map(t => currencySelectorPostRewardDAO(currency, t))
        .update(currencySelectorPostRewardDB(currency, post) + reward)
      case _ => DBIO.successful(0)
    }

  def findAccountWithRolesById(userId: Long): Future[Option[Account]] =
    updateAccountWithRoles(findAccountById(userId))

  def updateTxsToAccountsWithAccounts(t: models.Transaction): Future[models.Transaction] =
    t.toId match {
      case Some(id) =>
        findAccountById(id).map(_.fold(t) { u =>
          t.toAccountOpt = Some(u)
          t
        })
      case _ => Future(t)
    }

  def updateTxsFromAccountsWithAccounts(t: models.Transaction): Future[models.Transaction] =
    t.fromId match {
      case Some(id) =>
        findAccountById(id).map(_.fold(t) { u =>
          t.fromAccountOpt = Some(u)
          t
        })
      case _ => Future(t)
    }

  def updateAccountWithRoles(futureOptAccount: Future[Option[Account]]): Future[Option[Account]] =
    futureOptAccount flatMap {
      case Some(account) => findRolesByAccountId(account.id).map(r => Some {
        account.roles = r
        account
      })
      case None => Future.successful(None)
    }

  def findRolesByAccountId(userId: Long) =
    db.run(roles.filter(_.userId === userId).result).map(_.map(_.role))

  def getTransactionsByAccountIdWithInfo(userId: Long, pageId: Long, isSchelduled: Boolean): Future[Seq[models.Transaction]] = {
    findTransactionsByAccountId(userId, pageId, isSchelduled).flatMap { st =>
      Future.sequence(st.map(t => updateTxsFromAccountsWithAccounts(t).flatMap(updateTxsToAccountsWithAccounts)))
    }
  }

  def getTransactionsPagesByAccountIdWithInfo(userId: Long, isSchelduled: Boolean): Future[Int] = {
    if (isSchelduled)
      db.run(transactions
        .filter(t => ((t.fromType === models.TargetType.ACCOUNT && t.fromId === userId) ||
          (t.toType === models.TargetType.ACCOUNT && t.toId === userId)) && t.scheduled.isDefined).size.result) map pages
    else
      db.run(transactions
        .filter(t => (t.fromType === models.TargetType.ACCOUNT && t.fromId === userId) ||
          (t.toType === models.TargetType.ACCOUNT && t.toId === userId)).size.result) map pages
  }

  def findTransactionsByAccountId(userId: Long, pageId: Long, isSchelduled: Boolean): Future[Seq[models.Transaction]] = {
    val query = for {
      dbTx <- if (isSchelduled)
        transactions
          .filter(t => ((t.fromType === models.TargetType.ACCOUNT && t.fromId === userId) ||
            (t.toType === models.TargetType.ACCOUNT && t.toId === userId)) && t.scheduled.isDefined)
          .sortBy(_.id.desc).drop(if (pageId > 0) pageSize * (pageId - 1) else 0).take(pageSize)
      else
        transactions
          .filter(t => (t.fromType === models.TargetType.ACCOUNT && t.fromId === userId) ||
            (t.toType === models.TargetType.ACCOUNT && t.toId === userId))
          .sortBy(_.id.desc).drop(if (pageId > 0) pageSize * (pageId - 1) else 0).take(pageSize)
    } yield (dbTx)
    db.run(query.result).map(_ map transactionFrom)
  }

  def invalidateSessionBySessionKeyAndIP(sessionKey: String, ip: String): Future[Boolean] =
    db.run(sessions.filter(t => t.sessionKey === sessionKey && t.ip === ip).map(_.expire).update(System.currentTimeMillis).transactionally) map (r => if (r == 1) true else false)

  def updateAccountAbout(userId: Long, about: String): Future[Boolean] =
    db.run(accounts.filter(_.id === userId).map(_.about).update(Some(about)).transactionally) map (r => if (r == 1) true else false)

  def updateAccountBackground(userId: Long, bgURL: String): Future[Boolean] =
    db.run(accounts.filter(_.id === userId).map(_.background).update(Some(bgURL)).transactionally) map (r => if (r == 1) true else false)

  def updateAccountAvatar(userId: Long, avatarURL: String): Future[Boolean] =
    db.run(accounts.filter(_.id === userId).map(_.avatar).update(Some(avatarURL)).transactionally) map (r => if (r == 1) true else false)

  def updateAccountName(userId: Long, name: String): Future[Boolean] =
    db.run(accounts.filter(_.id === userId).map(_.name).update(Some(name)).transactionally) map (r => if (r == 1) true else false)

  def updateAccountSex(userId: Long, sexStr: String): Future[Boolean] =
    models.SexType.idByStr(sexStr) match {
      case Some(sex) => db.run(accounts.filter(_.id === userId).map(_.sex).update(Some(sex)).transactionally) map (r => if (r == 1) true else false)
      case _         => Future.successful(false)
    }

  def updateAccountBirthday(userId: Long, birthday: Long): Future[Boolean] =
    db.run(accounts.filter(_.id === userId).map(_.birthday).update(Some(birthday)).transactionally) map (r => if (r == 1) true else false)

  def updateAccountEducation(userId: Long, education: String): Future[Boolean] =
    db.run(accounts.filter(_.id === userId).map(_.education).update(Some(education)).transactionally) map (r => if (r == 1) true else false)

  def updateAccountSurname(userId: Long, surname: String): Future[Boolean] =
    db.run(accounts.filter(_.id === userId).map(_.surname).update(Some(surname)).transactionally) map (r => if (r == 1) true else false)

  def updateCompanyITN(companyId: Long, ITN: String): Future[Boolean] =
    db.run(accounts.filter(_.id === companyId).map(_.ITN).update(Some(ITN)).transactionally) map (r => if (r == 1) true else false)

  def updateCompanyIEC(companyId: Long, IEC: String): Future[Boolean] =
    db.run(accounts.filter(_.id === companyId).map(_.IEC).update(Some(IEC)).transactionally) map (r => if (r == 1) true else false)

  def createSession(
    userId:     Long,
    ip:         String,
    sessionKey: String,
    created:    Long,
    expire:     Long): Future[Option[models.Session]] = {
    val query = for {
      dbSession <- (sessions returning sessions.map(_.id) into ((v, id) => v.copy(id = id))) += new models.daos.DBSession(
        0,
        userId,
        ip,
        sessionKey,
        created,
        expire)
    } yield dbSession
    db.run(query.transactionally) map { dbSession =>
      Some(sessionFrom(dbSession))
    }
  }

  def getAccountsPages(pageSize: Long) =
    db.run(accounts.length.result).map { r =>
      pages(r, pageSize.toInt)
    }

  def getAccountBalancesPages(pageSize: Long) =
    db.run(balances.filter(t => t.ownerType === TargetType.ACCOUNT && t.balanceType === BalanceType.CURRENT).length.result).map { r =>
      pages(r, pageSize.toInt)
    }

  def findPreviousAccountBalance(ownerId: Long, currencyId: Int) =
    filterAccountBalance(ownerId, currencyId, BalanceType.PREVIOUS).result.head

  def filterPreviousAccountBalance(ownerId: Long, currencyId: Int) =
    filterAccountBalance(ownerId, currencyId, BalanceType.PREVIOUS)

  def getCurrentAccountBalanceValue(ownerId: Long, currencyId: Int): Future[Option[Long]] = 
    db.run(filterCurrentAccountBalanceValue(ownerId, currencyId).result.headOption)

  def filterCurrentAccountBalanceValue(ownerId: Long, currencyId: Int) =
    filterCurrentAccountBalance(ownerId, currencyId).map(_.value)

  def filterCurrentAccountBalance(ownerId: Long, currencyId: Int) =
    filterAccountBalance(ownerId, currencyId, BalanceType.CURRENT)

  def filterCurrentBatchBalance(ownerId: Long, currencyId: Int) =
    balances.filter(t =>
      t.ownerType === TargetType.BATCH &&
        t.balanceType === BalanceType.CURRENT &&
        t.ownerId === ownerId &&
        t.currencyId === currencyId)

  def filterCurrentCampaingBalance(ownerId: Long, currencyId: Int) =
    balances.filter(t =>
      t.ownerType === TargetType.CAMPAIGN &&
        t.balanceType === BalanceType.CURRENT &&
        t.ownerId === ownerId &&
        t.currencyId === currencyId)

  def findCurrentBatchBalance(ownerId: Long, currencyId: Int) =
    filterCurrentBatchBalance(ownerId, currencyId).result.head

  def findCurrentCampaingBalance(ownerId: Long, currencyId: Int) =
    filterCurrentCampaingBalance(ownerId, currencyId).result.head

  def updateCurrentCampaingBalance(ownerId: Long, currencyId: Int, delta: Long) =
    findCurrentCampaingBalance(ownerId, currencyId).flatMap { balance =>
      balances.filter(_.id === balance.id)
        .map(t => (t.value, t.updated))
        .update(balance.value + delta, System.currentTimeMillis())
    }

  def updateCurrentBatchBalance(ownerId: Long, currencyId: Int, delta: Long) =
    findCurrentBatchBalance(ownerId, currencyId).flatMap { balance =>
      balances.filter(_.id === balance.id)
        .map(t => (t.value, t.updated))
        .update(balance.value + delta, System.currentTimeMillis())
    }

  def filterAccountBalance(ownerId: Long, currencyId: Int, balanceType: Int) =
    balances.filter(t =>
      t.ownerType === TargetType.ACCOUNT &&
        t.balanceType === balanceType &&
        t.ownerId === ownerId &&
        t.currencyId === currencyId)

  def findCurrentAccountBalance(ownerId: Long, currencyId: Int) =
    filterCurrentAccountBalance(ownerId, currencyId).result.head

  def findCurrentAccountBalanceOpt(ownerId: Long, currencyId: Int) =
    filterCurrentAccountBalance(ownerId, currencyId).result.headOption

  def updateCurrentAccountBalance(ownerId: Long, currencyId: Int, delta: Long) =
    findCurrentAccountBalance(ownerId, currencyId).flatMap { balance =>
      balances.filter(_.id === balance.id)
        .map(t => (t.value, t.updated))
        .update(balance.value + delta, System.currentTimeMillis())
    }

  def updateBalances(pageSize: Long, pageId: Long): Future[Int] = {
    val timestamp = System.currentTimeMillis
    db.run((for {
      balancesToUpdate <- balances
        .filter(t => t.ownerType === TargetType.ACCOUNT && t.balanceType === BalanceType.CURRENT)
        .sortBy(_.id.desc).drop(if (pageId > 0) pageSize * (pageId - 1) else 0).take(pageSize).result
      updatedRows <- DBIO.sequence {
        balancesToUpdate.map { balance =>
          balances.filter(t =>
            t.currencyId === balance.currencyId &&
              t.ownerId === balance.ownerId &&
              t.ownerType === TargetType.ACCOUNT &&
              t.balanceType === BalanceType.PREVIOUS)
            .map(t => (t.value, t.updated)).update(balance.value, timestamp)
        }
      }
    } yield updatedRows).transactionally).map(_.sum)
  }

  def resetCounters(pageSize: Long, pageId: Long): Future[Int] = {
    val timestamp = System.currentTimeMillis
    db.run((for {
      accountsToUpdate <- accounts.sortBy(_.id.desc).drop(if (pageId > 0) pageSize * (pageId - 1) else 0).take(pageSize).result
      updatedRows <- DBIO.sequence {
        accountsToUpdate.map(account => accounts.filter(_.id === account.id).map(t => (
          t.likesCounter,
          t.commentsCounter,
          t.postsCounter,
          t.likesCounterStarted,
          t.postsCounterStarted,
          t.commentsCounterStarted)).update(
          0,
          0,
          0,
          timestamp,
          timestamp,
          timestamp))
      }
    } yield updatedRows).transactionally).map(_.sum)
  }

  def updateTaskExecutionTime(taskId: Long): Future[Boolean] =
    db.run(scheduledTasks.filter(_.id === taskId).map(_.executed).update(Some(System.currentTimeMillis))) map (_ == 1)

  def getTaskLastExecution(taskId: Long): Future[Long] =
    db.run(scheduledTasks.filter(_.id === taskId).result.headOption) map {
      case Some(scheduledTask) => scheduledTask.executed.getOrElse(0L)
      case _                   => 0L
    }

  def getActiveCampaingsPricesForProduct(productId: Long): Future[Seq[Long]] = {
    val timestamp = System.currentTimeMillis()
    db.run(getActiveCampaingsPricesForProductDB(productId))
  }

  def getActiveCampaingsForProductDB(productId: Long) = {
    val timestamp = System.currentTimeMillis()
    marketingCampaigns.filter(t =>
      t.count > 0 &&
        t.start <= timestamp &&
        t.end > timestamp &&
        t.status === models.MarketingCampaignStatus.ACTIVE &&
        t.productId === productId).sortBy(_.id.desc).take(AppConstants.ACTIVE_CAMPAINGS_LIMIT).result
  }

  def getActiveCampaingsPricesForProductDB(productId: Long) = {
    val timestamp = System.currentTimeMillis()
    marketingCampaigns.filter(t =>
      t.count > 0 &&
        t.start <= timestamp &&
        t.end > timestamp &&
        t.status === models.MarketingCampaignStatus.ACTIVE &&
        t.productId === productId).sortBy(_.id.desc).take(AppConstants.ACTIVE_CAMPAINGS_LIMIT).map(_.price).result
  }

  def getNotBoughtItemWithBatchByCodeDB(code: String) =
    items
      .filter(t => t.code === code && t.status === ItemStatus.READY && t.bought.isEmpty)
      .join(batches).on(_.batchId === _.id)
      .result.headOption

  def getNotBoughtItemWithBatchByCode(code: String): Future[Option[(models.Item, models.Batch)]] =
    db.run(getNotBoughtItemWithBatchByCodeDB(code)).map(_ map { case (item, batch) => (itemFrom(item), batchFrom(batch)) })

  def getBottleInfo(code: String): Future[Option[models.Item]] =
    db.run(items.filter(_.id === code.trim.toLong).result.headOption) map (_ map itemFrom)

  def scheduleNotification(code: String, userId: Long): Future[Boolean] = {
    val timestamp = System.currentTimeMillis()
    val prepCode = code.trim.toLong
    val query = for {
      itemOpt <- items.filter(t => t.id === prepCode && t.bought.isEmpty).result.headOption
      taskOpt <- itemOpt match {
        case Some(item) =>
          batches.filter(_.id === item.batchId).map(_.productId).result.head flatMap { productId =>
            scheduledTasks += new models.daos.DBScheduledTask(
              0,
              None,
              models.TaskType.NOTIFICATIONS,
              Some(timestamp + AppConstants.NOTIFICATION_INTERVAL),
              Some(userId),
              Some(productId))
          }
        case _ => DBIO.successful(0)
      }
    } yield (taskOpt)
    db.run(query.transactionally) map (_ == 1)
  }

  def checkItemAndUpdateGeo(code: String, geoOpt: Option[models.Geo]): Future[Option[models.Item]] = {
    val timestamp = System.currentTimeMillis()
    val prepCode = code.trim.toLong
    val query = for {
      itemOpt <- items.filter(t => t.id === prepCode).result.headOption
      _ <- (geoOpt, itemOpt) match {
        case (Some(geo), Some(item)) =>
          item.bought match {
            case Some(t) => DBIO.successful(0)
            case _ => positions += new models.daos.DBPosition(
              0,
              item.id,
              timestamp,
              geo.longitude,
              geo.latitude,
              geo.accuracy)
          }
        case _ => DBIO.successful(0)
      }
    } yield (itemOpt)
    db.run(query.transactionally) map (_ map itemFrom)
  }

  def createCampaing(
    productId: Long,
    price:     Long,
    count:     Int,
    start:     Long,
    end:       Long,
    descrOpt:  Option[String],
    balance:   Long,
    title:     String): Future[Option[models.MarketingCampaign]] = {
    val timestamp = System.currentTimeMillis
    val query = for {
      productOwnerId <- posts.filter(t => t.id === productId && t.postType === PostType.PRODUCT).map(_.ownerId).result.head
      account <- accounts.filter(_.id === productOwnerId).result.head
      accountBalance <- filterCurrentAccountBalanceValue(account.id, CurrencyType.DOLLAR).result.head
      campaignOpt <- if (accountBalance < balance) DBIO.successful(None) else
        ((marketingCampaigns returning marketingCampaigns.map(_.id) into ((v, id) => Some(v.copy(id = id)))) += new models.daos.DBMarketingCampaign(
          0,
          productId,
          count,
          count,
          price,
          start,
          end,
          models.MarketingCampaignStatus.ACTIVE,
          descrOpt,
          title)) flatMap (_ match {
          case Some(campaign) =>
            filterCurrentAccountBalance(account.id, CurrencyType.DOLLAR)
              .map(t => (t.updated, t.value))
              .update(timestamp, accountBalance - balance)
              .flatMap(_ => (balances += new DBBalance(
                0,
                campaign.id,
                TargetType.CAMPAIGN,
                CurrencyType.DOLLAR,
                timestamp,
                BalanceType.CURRENT,
                balance))
                .flatMap(_ => (transactions += new models.daos.DBTransaction(
                  0,
                  timestamp,
                  None,
                  Some(timestamp),
                  TargetType.ACCOUNT,
                  TargetType.CAMPAIGN,
                  Some(account.id),
                  Some(campaign.id),
                  None,
                  None,
                  None,
                  None,
                  None,
                  None,
                  TxType.CREATE_CAMPAIGN_DEPOSIT,
                  None,
                  TxState.APPROVED,
                  CurrencyType.DOLLAR,
                  balance)).flatMap(_ => DBIO.successful(Some(campaign)))))
          case _ => DBIO.successful(None)
        })
    } yield (campaignOpt)
    db.run(query.transactionally).map(_ map marketingCampaignFrom)
  }

  def transfer(fromAccountId: Long, toAccountId: Long, currency: Int, amount: Long, msgOpt: Option[String]): Future[Option[models.Transaction]] =
    db.run((for {
      tx <- (transactions returning transactions.map(_.id) into ((v, id) => Some(v.copy(id = id)))) += new models.daos.DBTransaction(
        0,
        System.currentTimeMillis,
        None,
        Some(System.currentTimeMillis),
        TargetType.ACCOUNT,
        TargetType.ACCOUNT,
        Some(fromAccountId),
        Some(toAccountId),
        None,
        None,
        None,
        None,
        None,
        None,
        TxType.USER_TO_USER,
        msgOpt,
        TxState.APPROVED,
        currency,
        amount)
      _ <- updateCurrentAccountBalance(fromAccountId, currency, -amount)
      _ <- updateCurrentAccountBalance(toAccountId, currency, amount)
    } yield (tx)).transactionally) map (_.map(transactionFrom))

  def promote(fromAccountId: Long, post: models.Post, amount: Long, msgOpt: Option[String]): Future[Option[models.Transaction]] =
    db.run((for {
      _ <- posts.filter(_.id === post.id).map(_.promo).update(post.promo + amount)
      tx <- (transactions returning transactions.map(_.id) into ((v, id) => Some(v.copy(id = id)))) += new models.daos.DBTransaction(
        0,
        System.currentTimeMillis,
        None,
        Some(System.currentTimeMillis),
        TargetType.ACCOUNT,
        TargetType.SYSTEM,
        Some(fromAccountId),
        None,
        None,
        Some(TargetType.POST),
        None,
        Some(post.id),
        None,
        None,
        TxType.PROMOTE_POST,
        msgOpt,
        TxState.APPROVED,
        CurrencyType.DOLLAR,
        amount)
      _ <- updateCurrentAccountBalance(fromAccountId, CurrencyType.DOLLAR, -amount)
    } yield (tx)).transactionally) map (_.map(transactionFrom))

  def chargeBalance(accountId: Long, currencyId: Int, value: Long): Future[Option[Long]] = {
    if(value > 0)
      addValueToAccountBalance(
        TxType.CHARGE,
        accountId,
        value,
        currencyId,
        Some("Charge balance from system"),
        None,
        None,
        None,
        None) flatMap ( _.fold(Future.successful[Option[Long]](None)) { tx =>
         getCurrentAccountBalanceValue(accountId, currencyId)
      })
    else
      removeValueFromAccountBalance(
        TxType.CHARGE,
        accountId,
        value,
        currencyId,
        Some("Remove balance from account to system"),
        None,
        None,
        None,
        None) flatMap ( _.fold(Future.successful[Option[Long]](None)) { tx =>
         getCurrentAccountBalanceValue(accountId, currencyId)
      })
  }

  def removeValueFromAccountBalance(
    txType:           Int,
    userId:           Long,
    amount:           Long,
    currency:         Int,
    msg:              Option[String],
    fromRouteTypeOpt: Option[Int],
    fromRouteIdOpt:   Option[Long],
    toRouteTypeOpt:   Option[Int],
    toRouteIdOpt:     Option[Long]): Future[Option[models.Transaction]] =
    db.run(removeValueFromAccountBalanceDBIOAction(
      txType,
      userId,
      amount,
      currency,
      msg,
      TargetType.SYSTEM,
      None,
      fromRouteTypeOpt,
      fromRouteIdOpt,
      toRouteTypeOpt,
      toRouteIdOpt).transactionally)
      .map(_.map(transactionFrom))

  def addValueToAccountBalance(
    txType:           Int,
    userId:           Long,
    amount:           Long,
    currency:         Int,
    msg:              Option[String],
    fromRouteTypeOpt: Option[Int],
    fromRouteIdOpt:   Option[Long],
    toRouteTypeOpt:   Option[Int],
    toRouteIdOpt:     Option[Long]): Future[Option[models.Transaction]] =
    db.run(addValueToAccountBalanceDBIOAction(
      txType,
      userId,
      amount,
      currency,
      msg,
      TargetType.SYSTEM,
      None,
      fromRouteTypeOpt,
      fromRouteIdOpt,
      toRouteTypeOpt,
      toRouteIdOpt).transactionally)
      .map(_.map(transactionFrom))

  def findPostByComment(commentId: Long): Future[Option[models.Post]] = {
    val query = for {
      dbComment <- comments.filter(_.id === commentId)
      dbPost <- posts.filter(_.id === dbComment.postId)
    } yield dbPost
    db.run(query.result.headOption).map(_.map(postFrom))
  }

  def createLikeToPostWithLikesCounterUpdate(
    userId:       Long,
    postId:       Long,
    commentIdOpt: Option[Long]): Future[Option[models.Like]] = {
    commentIdOpt.fold(createLikeToPostWithLikesCounterUpdate(
      userId,
      postId))(commentId => createLikeToPostWithLikesCounterUpdate(
      userId,
      postId,
      commentId))
  }

  def addRewardOpt(
    txType:          Int,
    userId:          Long,
    value:           Long,
    currency:        Int,
    msg:             Option[String],
    fromType:        Int,
    fromId:          Option[Long],
    routeTargetType: Option[Int],
    routeTargetId:   Option[Long]) =
    if (value > 0)
      addValueToAccountBalanceDBIOAction(
        txType,
        userId,
        value,
        currency,
        msg,
        fromType,
        fromId,
        routeTargetType,
        routeTargetId,
        None,
        None)
    else
      DBIO.successful(None)

  def createLikeToPostWithLikesCounterUpdate(
    userId:    Long,
    postId:    Long,
    commentId: Long): Future[Option[models.Like]] = {

    val query = for {
      like <- (likes returning likes.map(_.id) into ((v, id) => v.copy(id = id))) += new models.daos.DBLike(
        0,
        userId,
        TargetType.COMMENT,
        commentId,
        System.currentTimeMillis)
      account <- accounts.filter(_.id === userId).result.head
      _ <- accounts.filter(_.id === userId).map(_.likesCounter).update(account.likesCounter + 1)
      userPrevPowerBalance <- findPreviousAccountBalance(account.id, CurrencyType.POWER).map(_.value)
      // rewards to liker - 0.5*DP
      _ <- addRewardOpt(
        TxType.LIKER_REWARD,
        userId,
        RewardLogic.likerRewardByLikePower(userPrevPowerBalance),
        CurrencyType.POWER,
        Some("Account POWER reward for like creation"),
        TargetType.SYSTEM,
        None,
        Some(TargetType.POST),
        Some(postId))
      // rewards to comment owner 0.75*DP of actor
      comment <- comments.filter(_.id === commentId).result.head
      _ <- comments.filter(_.id === commentId).map(_.likesCount).update(comment.likesCount + 1)
      commentOwner <- accounts.filter(_.id === comment.ownerId).result.head
      _ <- {
        val reward = RewardLogic.postRewardByLikePower(userPrevPowerBalance, models.RewardType.POWER)
        addRewardOpt(
          TxType.LIKE_REWARD,
          commentOwner.id,
          reward,
          CurrencyType.POWER,
          Some("Comment owner POWER reward by another user like"),
          TargetType.SYSTEM,
          None,
          Some(TargetType.COMMENT),
          Some(commentId)) flatMap (t => udateCommentReward(t, comment, reward, CurrencyType.POWER))
      }
      _ <- addRewardOpt(
        TxType.LIKER_REWARD,
        commentOwner.id,
        RewardLogic.likerRewardByLikePower(userPrevPowerBalance),
        CurrencyType.POWER,
        Some("Liker POWER reward"),
        TargetType.SYSTEM,
        None,
        Some(TargetType.COMMENT),
        Some(commentId))
      userActual <- accounts.filter(_.id === userId).result.head
      (rewardPower, rewardDollar, rewardToken) <- comments.filter(_.id === commentId).map(t => (t.rewardPower, t.rewardDollar, t.rewardToken)).result.head
    } yield (like, rewardPower, rewardDollar, rewardToken, userActual)

    db.run(query.transactionally) map {
      case (like, rewardPower, rewardDollar, rewardToken, userActual) =>
        Some {
          val likeModel = likeFrom(like)
          likeModel.rewardOpt = Some(new models.Reward(Some(rewardPower)))
          likeModel.ownerOpt = Some(accountFrom(userActual))
          likeModel.userLoginOpt = Some(userActual.login)
          likeModel.displayNameOpt = userActual.userType match {
            case AccountType.COMPANY => userActual.name
            case _                   => Some(userActual.login)
          }
          likeModel
        }
    }
  }

  // FIXME: check for balance must be atomic. But now in DB filed can't be negative.
  def exchange(user: models.Account, currencyFrom: Int, currencyTo: Int, value: Long): Future[Boolean] = {
    val timestamp = System.currentTimeMillis
    val (scheduledOpt, approvedOpt, txState, amount) =
      (currencyFrom, currencyTo) match {
        case (CurrencyType.DOLLAR, CurrencyType.TOKEN) =>
          (Some(timestamp + AppConstants.DOLLAR_TO_TOKEN_CHANGE_INTERVAL), None, TxState.SCHEDULED, RewardLogic.DOLLARInTOKEN(value))
        case (CurrencyType.POWER, CurrencyType.TOKEN) =>
          (Some(timestamp + AppConstants.POWER_TO_TOKEN_CHANGE_INTERVAL), None, TxState.SCHEDULED, RewardLogic.POWERInTOKEN(value))
        case (CurrencyType.TOKEN, CurrencyType.POWER) =>
          (None, Some(timestamp), TxState.APPROVED, RewardLogic.TOKENInPOWER(value))
        case (CurrencyType.TOKEN, CurrencyType.DOLLAR) =>
          (None, Some(timestamp), TxState.APPROVED, RewardLogic.TOKENInDOLLAR(value))
        case _ => throw new UnsupportedOperationException("Unsupported exchange")
      }
    val query = for {
      txMinus <- (transactions returning transactions.map(_.id) into ((v, id) => Some(v.copy(id = id)))) += new models.daos.DBTransaction(
        0,
        timestamp,
        None,
        Some(timestamp),
        TargetType.ACCOUNT,
        TargetType.SYSTEM,
        Some(user.id),
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        TxType.EXCHANGE_FROM,
        Some("Exhange withdraw"),
        TxState.APPROVED,
        currencyFrom,
        value)
      txPlus <- (transactions returning transactions.map(_.id) into ((v, id) => Some(v.copy(id = id)))) += new models.daos.DBTransaction(
        0,
        timestamp,
        scheduledOpt,
        approvedOpt,
        TargetType.SYSTEM,
        TargetType.ACCOUNT,
        None,
        Some(user.id),
        None,
        None,
        None,
        None,
        None,
        None,
        TxType.EXCHANGE_TO,
        Some("Exhange charge"),
        txState,
        currencyTo,
        amount)
      // withdraw
      _ <- updateCurrentAccountBalance(user.id, currencyFrom, -value)
      _ <- approvedOpt match {
        case Some(approveTime) =>
          updateCurrentAccountBalance(user.id, currencyTo, amount)
        case _ => DBIO.successful(0)
      }
    } yield (txMinus, txPlus)
    db.run(query.transactionally) map (_ match {
      case (Some(txm), Some(txp)) => true
      case _                      => false
    })
  }

  def createLikeToPostWithLikesCounterUpdate(
    userId: Long,
    postId: Long): Future[Option[models.Like]] = {
    val query = for {
      like <- (likes returning likes.map(_.id) into ((v, id) => v.copy(id = id))) += new models.daos.DBLike(
        0,
        userId,
        TargetType.POST,
        postId,
        System.currentTimeMillis)
      user <- accounts.filter(_.id === userId).result.head
      _ <- accounts.filter(_.id === userId).map(_.likesCounter).update(user.likesCounter + 1)
      userPrevPowerBalance <- findPreviousAccountBalance(userId, CurrencyType.POWER).map(_.value)
      // rewards to liker - 0.5*DP
      _ <- {
        val reward = RewardLogic.likerRewardByLikePower(userPrevPowerBalance)
        if (reward > 0) {
          addValueToAccountBalanceDBIOAction(
            TxType.LIKER_REWARD,
            userId,
            reward,
            CurrencyType.POWER,
            Some("Account POWER reward for like creation"),
            TargetType.SYSTEM,
            None,
            Some(TargetType.POST),
            Some(postId),
            None,
            None)
        } else DBIO.successful(None)
      }
      // rewards to post owner 0.75*DP of actor
      post <- posts.filter(_.id === postId).result.head
      _ <- posts.filter(_.id === postId).map(_.likesCount).update(post.likesCount + 1)
      postOwner <- accounts.filter(_.id === post.ownerId).result.head
      _ <- {
        val reward = RewardLogic.postRewardByLikePower(userPrevPowerBalance, post.rewardType)
        addRewardOpt(
          TxType.LIKE_REWARD,
          postOwner.id,
          reward,
          CurrencyType.POWER,
          Some("Post owner POWER reward by another user like"),
          TargetType.SYSTEM,
          None,
          Some(TargetType.POST),
          Some(postId)) flatMap (t => udatePostReward(t, post, reward, CurrencyType.POWER))
      }
      _ <- {
        val reward = RewardLogic.postRewardByLikeDollars(userPrevPowerBalance, post.rewardType)
        addRewardOpt(
          TxType.LIKE_REWARD,
          postOwner.id,
          reward,
          CurrencyType.DOLLAR,
          Some("Post owner DOLLAR reward by another user like"),
          TargetType.SYSTEM,
          None,
          Some(TargetType.POST),
          Some(postId)) flatMap (t => udatePostReward(t, post, reward, CurrencyType.DOLLAR))
      }
      (rewardPower, rewardDollar, rewardToken) <- posts.filter(_.id === postId).map(t => (t.rewardPower, t.rewardDollar, t.rewardToken)).result.head
    } yield (like, rewardPower, rewardDollar, rewardToken, user)

    db.run(query.transactionally) map {
      case (like, rewardPower, rewardDollar, rewardToken, user) =>
        Some {
          val likeModel = likeFrom(like)
          likeModel.rewardOpt = Some(new models.Reward(Some(rewardPower), Some(rewardDollar)))
          likeModel.ownerOpt = Some(accountFrom(user))
          likeModel.userLoginOpt = Some(user.login)
          likeModel.displayNameOpt = user.userType match {
            case AccountType.COMPANY => user.name
            case _                   => Some(user.login)
          }
          likeModel
        }
    }
  }

  def approveTransaction(txId: Long): Future[Option[models.Transaction]] = {
    val timestamp = System.currentTimeMillis
    val query = for {
      _ <- transactions.filter(t => t.id === txId && t.scheduled.isDefined && t.scheduled <= timestamp && t.toType === TargetType.ACCOUNT)
        .map(t => (t.scheduled, t.state, t.processed))
        .update(None, TxState.APPROVED, Some(timestamp))
      tx <- transactions.filter(_.id === txId).result.head
      user <- accounts.filter(_.id === tx.toId.get).result.head
      _ <- updateCurrentAccountBalance(user.id, tx.currencyId, tx.amount)
    } yield tx
    db.run(query.transactionally).map(t => Some(transactionFrom(t)))
  }

  def findTransactionById(txId: Long): Future[Option[models.Transaction]] =
    db.run(transactions.filter(_.id === txId).result.headOption).map(_.map(transactionFrom))

  def createCommentToPostWithCommentsCounterUpdate(
    postId:       Long,
    commentIdOpt: Option[Long],
    userId:       Long,
    content:      String,
    rewardType:   Int): Future[Option[models.Comment]] = {

    val query = for {
      userOpt <- accounts.filter(_.id === userId).result.headOption
      commentOpt <- DBIO.sequenceOption(userOpt.map { user =>
        (comments returning comments.map(_.id) into ((v, id) => v.copy(id = id))) += new models.daos.DBComment(
          0,
          postId,
          userId,
          commentIdOpt,
          content,
          models.ContentType.TEXT,
          System.currentTimeMillis,
          0, 0, 0, 0, models.CommentStatus.VISIBLE)
      })
      _ <- userOpt match {
        case Some(user) =>
          accounts.filter(_.id === userId).map(_.commentsCounter).update(user.commentsCounter + 1)
        case _ => DBIO.successful(None)
      }

      post <- posts.filter(_.id === postId).result.head

      _ <- posts.filter(_.id === postId).map(_.commentsCount).update(post.commentsCount + 1)

      actualCommentOpt <- commentOpt match {
        case Some(comment) =>
          comments.filter(_.id === comment.id).result.headOption
        case _ => DBIO.successful(None)
      }
      actualAccountOpt <- accounts.filter(_.id === userId).result.headOption
    } yield (actualCommentOpt, actualAccountOpt)

    db.run(query.transactionally) map {
      case (Some(actualComment), Some(user)) =>
        val comment = commentFrom(actualComment)
        comment.ownerOpt = Some(accountFrom(user))
        Some(comment)
      case _ => None
    }
  }

  def removeValueFromAccountBalanceDBIOAction(
    txType:           Int,
    userId:           Long,
    amount:           Long,
    currency:         Int,
    msg:              Option[String],
    targetType:         Int,
    toId:             Option[Long],
    fromRouteTypeOpt: Option[Int],
    fromRouteIdOpt:   Option[Long],
    toRouteTypeOpt:   Option[Int],
    toRouteIdOpt:     Option[Long]) =
    for {
      tx <- (transactions returning transactions.map(_.id) into ((v, id) => Some(v.copy(id = id)))) += new models.daos.DBTransaction(
        0,
        System.currentTimeMillis,
        None,
        Some(System.currentTimeMillis),
        TargetType.ACCOUNT,
        targetType,
        Some(userId),
        toId,
        fromRouteTypeOpt,
        toRouteTypeOpt,
        fromRouteIdOpt,
        toRouteIdOpt,
        None,
        None,
        txType,
        msg,
        TxState.APPROVED,
        currency,
        -amount)
      _ <- updateCurrentAccountBalance(userId, currency, amount)
    } yield tx


  def addValueToAccountBalanceDBIOAction(
    txType:           Int,
    userId:           Long,
    amount:           Long,
    currency:         Int,
    msg:              Option[String],
    fromType:         Int,
    fromId:           Option[Long],
    fromRouteTypeOpt: Option[Int],
    fromRouteIdOpt:   Option[Long],
    toRouteTypeOpt:   Option[Int],
    toRouteIdOpt:     Option[Long]) =
    for {
      tx <- (transactions returning transactions.map(_.id) into ((v, id) => Some(v.copy(id = id)))) += new models.daos.DBTransaction(
        0,
        System.currentTimeMillis,
        None,
        Some(System.currentTimeMillis),
        fromType,
        TargetType.ACCOUNT,
        fromId,
        Some(userId),
        fromRouteTypeOpt,
        toRouteTypeOpt,
        fromRouteIdOpt,
        toRouteIdOpt,
        None,
        None,
        txType,
        msg,
        TxState.APPROVED,
        currency,
        amount)
      _ <- updateCurrentAccountBalance(userId, currency, amount)
    } yield tx

  def findAccountByConfirmCodeAndLogin(login: String, code: String): Future[Option[models.Account]] =
    getAccountFromQuery(accounts.filter(t => t.login === login && t.confirmCode === code))

  def emailVerified(login: String, code: String, password: String): Future[Option[Account]] =
    db.run(accounts.filter(t => t.login === login && t.confirmCode === code)
      .map(t => (t.confirmCode, t.accountStatus, t.hash))
      .update(None, AccountStatus.CONFIRMED, Some(BCrypt.hashpw(password, BCrypt.gensalt())))).flatMap { raws =>
      if (raws == 1) findAccountByLogin(login) else Future.successful(None)
    }

  def disableShowBrand(id: Long): Future[Boolean] =
    db.run(accounts.filter(_.id === id).map(_.isShowBrand).update(false).transactionally).map(_ > 0)

  def createAccount(
    login:          String,
    email:          String,
    balanceDp:      Long,
    userType:       Int,
    companyNameOpt: Option[String],
    ITNOpt:         Option[String],
    IECOpt:         Option[String]): Future[Option[models.Account]] = {
    val timestamp = System.currentTimeMillis()
    val query = for {
      dbAccount <- (accounts returning accounts.map(_.id) into ((v, id) => v.copy(id = id))) += new models.daos.DBAccount(
        0,
        login,
        email,
        None /*Some(BCrypt.hashpw(password, BCrypt.gensalt()))*/ ,
        None,
        None,
        UserStatus.NORMAL,
        AccountStatus.WAITE_CONFIRMATION,
        companyNameOpt, None, None, 0, System.currentTimeMillis,
        Some(BCrypt.hashpw(Random.nextString(5) + login + System.currentTimeMillis.toString, BCrypt.gensalt())
          .replaceAll("\\.", "s")
          .replaceAll("\\\\", "d")
          .replaceAll("\\$", "g").toList.map(_.toInt.toHexString).mkString.substring(0, 99)), 0, 0, 0, 0, 0, 0,
        0,
        None,
        true,
        ITNOpt,
        IECOpt,
        None,
        None,
        None,
        0,
        userType,
        None,
        None,
        None,
        0)
      dbTx <- (transactions returning transactions.map(_.id) into ((v, id) => v.copy(id = id))) += new models.daos.DBTransaction(
        0,
        System.currentTimeMillis,
        None,
        Some(System.currentTimeMillis),
        TargetType.SYSTEM,
        TargetType.ACCOUNT,
        None,
        Some(dbAccount.id),
        None,
        None,
        None,
        None,
        None,
        None,
        TxType.REGISTER_REWARD,
        Some("Account POWER reward for registration"),
        TxState.APPROVED,
        CurrencyType.POWER,
        balanceDp)
      _ <- balances += new DBBalance(0, dbAccount.id, TargetType.ACCOUNT, CurrencyType.POWER, timestamp, BalanceType.CURRENT, balanceDp)
      _ <- balances += new DBBalance(0, dbAccount.id, TargetType.ACCOUNT, CurrencyType.POWER, timestamp, BalanceType.PREVIOUS, balanceDp)
      _ <- balances += new DBBalance(0, dbAccount.id, TargetType.ACCOUNT, CurrencyType.TOKEN, timestamp, BalanceType.CURRENT, 0)
      _ <- balances += new DBBalance(0, dbAccount.id, TargetType.ACCOUNT, CurrencyType.TOKEN, timestamp, BalanceType.PREVIOUS, 0)
      _ <- balances += new DBBalance(0, dbAccount.id, TargetType.ACCOUNT, CurrencyType.DOLLAR, timestamp, BalanceType.CURRENT, 0)
      _ <- balances += new DBBalance(0, dbAccount.id, TargetType.ACCOUNT, CurrencyType.DOLLAR, timestamp, BalanceType.PREVIOUS, 0)
    } yield (dbAccount, dbTx)
    db.run(query.transactionally) flatMap {
      case (dbAccount, dbTx) =>
        addRolesToAccount(dbAccount.id, Roles.CLIENT) map (t => Some(accountFrom(dbAccount, models.Roles.CLIENT)))
    }
  }

  def getStats: Future[models.Stats] = {
    val query = for {
      dbAccounts <- accounts.length.result
      dbUsers <- accounts.filter(_.userType === models.AccountType.USER).length.result
      dbCompanies <- accounts.filter(_.userType === models.AccountType.COMPANY).length.result
      dbTokens <- balances.filter(t => t.balanceType === BalanceType.CURRENT && t.currencyId === CurrencyType.TOKEN).map(_.value).sum.result
      dbDollars <- balances.filter(t => t.balanceType === BalanceType.CURRENT && t.currencyId === CurrencyType.DOLLAR).map(_.value).sum.result
      dbPower <- balances.filter(t => t.balanceType === BalanceType.CURRENT && t.currencyId === CurrencyType.POWER).map(_.value).sum.result
      dbPosts <- posts.length.result
      dbArticles <- posts.filter(_.postType === PostType.ARTICLE).length.result
      dbReviews <- posts.filter(_.postType === PostType.REVIEW).length.result
      dbProducts <- posts.filter(_.postType === PostType.PRODUCT).length.result

    } yield (dbAccounts, dbUsers, dbCompanies, dbTokens, dbDollars, dbPower, dbPosts, dbArticles, dbReviews, dbProducts)
    db.run(query) map {
      case (dbAccounts, dbUsers, dbCompanies, dbTokens, dbDollars, dbPower, dbPosts, dbArticles, dbReviews, dbProducts) =>
        new models.Stats(dbAccounts, dbUsers, dbCompanies, dbTokens.getOrElse(0), dbDollars.getOrElse(0), dbPower.getOrElse(0), dbPosts, dbArticles, dbReviews, dbProducts)
    }
  }

  def addRolesToAccount(userId: Long, rolesIn: Int*): Future[Unit] =
    db.run(DBIO.seq(roles ++= rolesIn.map(r => DBRole(userId, r))).transactionally)

  ////////////// HELPERS ////////////////

  @inline final def someToSomeFlatMap[T1, T2](f1: Future[Option[T1]], f2: T1 => Future[Option[T2]]): Future[Option[T2]] =
    f1 flatMap (_ match {
      case Some(r) => f2(r)
      case None    => Future.successful(None)
    })

  @inline final def someToSomeFlatMapElse[T](f1: Future[Option[_]], f2: Future[Option[T]]): Future[Option[T]] =
    f1 flatMap (_ match {
      case Some(r) => Future.successful(None)
      case None    => f2
    })

  @inline final def someToBooleanFlatMap[T](f1: Future[Option[T]], f2: T => Future[Boolean]): Future[Boolean] =
    f1 flatMap (_ match {
      case Some(r) => f2(r)
      case None    => Future.successful(false)
    })

  @inline final def someToSeqFlatMap[T1, T2](f1: Future[Option[T1]], f2: T1 => Future[Seq[T2]]): Future[Seq[T2]] =
    f1 flatMap (_ match {
      case Some(r) => f2(r)
      case None    => Future.successful(Seq.empty[T2])
    })

  @inline final def seqToSeqFlatMap[T1, T2](f1: Future[Seq[T1]], f2: T1 => Future[T2]): Future[Seq[T2]] =
    f1 flatMap { rs =>
      Future.sequence {
        rs map { r =>
          f2(r)
        }
      }
    }

}


