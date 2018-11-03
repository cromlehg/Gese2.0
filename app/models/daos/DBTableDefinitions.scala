package models.daos

import slick.jdbc.JdbcProfile
import slick.lifted.ProvenShape.proveShapeOf

trait DBTableDefinitions {

  protected val driver: JdbcProfile
  import driver.api._

  class Roles(tag: Tag) extends Table[DBRole](tag, "roles") {
    def userId = column[Long]("user_id")
    def role = column[Int]("role")
    def * = (userId, role) <> (DBRole.tupled, DBRole.unapply)
  }

  val roles = TableQuery[Roles]
  
  class Positions(tag: Tag) extends Table[DBPosition](tag, "positions") {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def itemId = column[Long]("item_id")
    def timestamp = column[Long]("timestamp")
    def position = column[String]("position")
    def longitude = column[Double]("longitude")
    def latitude = column[Double]("latitude")
    def accuracy = column[Double]("accuracy")
    def * = (id, itemId, timestamp, longitude, latitude, accuracy) <> (DBPosition.tupled, DBPosition.unapply)
  }

  val positions = TableQuery[Positions]
  
  class ScheduledTasks(tag: Tag) extends Table[DBScheduledTask](tag, "scheduled_tasks") {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def executed = column[Option[Long]]("executed")
    def taskType = column[Int]("task_type")
    def planned = column[Option[Long]]("planned")
    def accountId = column[Option[Long]]("account_id")
    def productId = column[Option[Long]]("product_id")
    def * = (
        id, 
        executed,
        taskType,
        planned,
        accountId,
        productId) <> (DBScheduledTask.tupled, DBScheduledTask.unapply)
  }

  val scheduledTasks = TableQuery[ScheduledTasks]

  class Comments(tag: Tag) extends Table[DBComment](tag, "comments") {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def postId = column[Long]("post_id")
    def ownerId = column[Long]("owner_id")
    def parentId = column[Option[Long]]("parent_id")
    def content = column[String]("content")
    def contentType = column[Int]("content_type")
    def created = column[Long]("created")
    def likesCount = column[Int]("likes_count")
    def rewardToken = column[Long]("reward_token")
    def rewardPower = column[Long]("reward_power")
    def rewardDollar = column[Long]("reward_dollar")
    def status = column[Int]("status")
    def * = (
        id, 
        postId, 
        ownerId, 
        parentId, 
        content, 
        contentType, 
        created, 
        likesCount,
        rewardToken,
        rewardPower,
        rewardDollar,
        status) <> (DBComment.tupled, DBComment.unapply)
  }

  val comments = TableQuery[Comments]
  
  class Currencies(tag: Tag) extends Table[DBCurrency](tag, "currencies") {
    def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
    def ticker = column[String]("ticker")
    def name = column[String]("name")
    def * = (id, ticker, name) <> (DBCurrency.tupled, DBCurrency.unapply)
  }

  val currencies = TableQuery[Currencies]
  
  class Balances(tag: Tag) extends Table[DBBalance](tag, "balances") {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def ownerId = column[Long]("owner_id")
    def ownerType = column[Int]("owner_type")
    def currencyId = column[Int]("currency_id")
    def updated = column[Long]("updated")
    def balanceType = column[Int]("balance_type")
    def value = column[Long]("value")
    def * = (id, ownerId, ownerType, currencyId, updated, balanceType, value) <> (DBBalance.tupled, DBBalance.unapply)
  }

  val balances = TableQuery[Balances]

  class Tags(tag: Tag) extends Table[DBTag](tag, "tags") {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def name = column[String]("name")
    def * = (id, name) <> (DBTag.tupled, DBTag.unapply)
  }

  val tags = TableQuery[Tags]
  
  class TagsToTargets(tag: Tag) extends Table[DBTagToTarget](tag, "tags_to_targets") {
    def tagId = column[Long]("tag_id")
    def targetId = column[Long]("target_id")
    def targetType = column[Int]("target_type")
    def created = column[Long]("created")
    def * = (tagId, targetId, targetType, created) <> (DBTagToTarget.tupled, DBTagToTarget.unapply)
  }
  
  val tagsToTargets = TableQuery[TagsToTargets]
  
  class Wallets(tag: Tag) extends Table[DBWallet](tag, "wallets") {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def ownerId = column[Option[Long]]("owner_id")
    def ownerTypeId = column[Int]("owner_type_id")
    def baseCurrencyId = column[Int]("base_currency_id")
    def address = column[String]("address")
    def privateKey = column[String]("private_key")
    def * = (id, ownerId, ownerTypeId, baseCurrencyId, address, privateKey) <> (DBWallet.tupled, DBWallet.unapply)
  }

  val wallets = TableQuery[Wallets]
  
  class Sessions(tag: Tag) extends Table[DBSession](tag, "sessions") {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def userId = column[Long]("user_id")
    def ip = column[String]("ip")
    def sessionKey = column[String]("session_key")
    def created = column[Long]("created")
    def expire = column[Long]("expire")
    def * = (id, userId, ip, sessionKey, created, expire) <> (DBSession.tupled, DBSession.unapply)
  }

  val sessions = TableQuery[Sessions]

  class Accounts(tag: Tag) extends Table[DBAccount](tag, "accounts") {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def login = column[String]("login")
    def email = column[String]("email")
    def hash = column[Option[String]]("hash")
    def avatar = column[Option[String]]("avatar")
    def background = column[Option[String]]("background")
    def userStatus = column[Int]("user_status")
    def accountStatus = column[Int]("account_status")
    def name = column[Option[String]]("name")
    def surname = column[Option[String]]("surname")
    def platformEth = column[Option[String]]("platform_eth")
    def timezoneId = column[Int]("timezone_id")
    def registered = column[Long]("registered")
    def confirmCode = column[Option[String]]("confirm_code")
    def postsCounter = column[Int]("posts_counter")
    def postsCounterStarted = column[Long]("posts_counter_started")
    def likesCounter = column[Int]("likes_counter")
    def likesCounterStarted = column[Long]("likes_counter_started")
    def commentsCounter = column[Int]("comments_counter")
    def commentsCounterStarted = column[Long]("comments_counter_started")
    def postsCount = column[Long]("posts_count")
    def about = column[Option[String]]("about")
    def isShowBrand = column[Boolean]("is_show_brand")
    def ITN = column[Option[String]]("ITN")
    def IEC = column[Option[String]]("IEC")
    def phones = column[Option[String]]("phones")
    def gender = column[Option[String]]("gender")
    def age = column[Option[Int]]("age")
    def rate = column[Int]("rate")
    def userType = column[Int]("user_type")
    def sex = column[Option[Int]]("sex")
    def birthday = column[Option[Long]]("birthday")
    def education = column[Option[String]]("education")
    def bought = column[Int]("bought")
    def * = ((
      id,
      login,
      email,
      hash,
      avatar,
      background,
      userStatus,
      accountStatus,
      name,
      surname,
      platformEth,
      timezoneId,
      registered,
      confirmCode,
      postsCounter,
      postsCounterStarted,
      likesCounter,
      likesCounterStarted,
      commentsCounter,
      commentsCounterStarted),( 
        postsCount,
        about,
        isShowBrand,
        ITN,
        IEC,
        phones,
        gender,
        age,
        rate,
        userType,
        sex,
        birthday,
        education,
        bought)) <> [DBAccount](t =>
          DBAccount(
            t._1._1,
            t._1._2,
            t._1._3,
            t._1._4,
            t._1._5,
            t._1._6,
            t._1._7,
            t._1._8,
            t._1._9,
            t._1._10,
            t._1._11,
            t._1._12,
            t._1._13,
            t._1._14,
            t._1._15,
            t._1._16,
            t._1._17,
            t._1._18,
            t._1._19,
            t._1._20,
            t._2._1,
            t._2._2,
            t._2._3,
            t._2._4,
            t._2._5,
            t._2._6,
            t._2._7,
            t._2._8,
            t._2._9,
            t._2._10,
            t._2._11,
            t._2._12,
            t._2._13,
            t._2._14), t => Some(
     (t.id,
      t.login,
      t.email,
      t.hash,
      t.avatar,
      t.background,
      t.userStatus,
      t.accountStatus,
      t.name,
      t.surname,
      t.platformEth,
      t.timezoneId,
      t.registered,
      t.confirmCode,
      t.postsCounter,
      t.postsCounterStarted,
      t.likesCounter,
      t.likesCounterStarted,
      t.commentsCounter,
      t.commentsCounterStarted),( 
        t.postsCount,
        t.about,
        t.isShowBrand,
        t.ITN,
        t.IEC,
        t.phones,
        t.gender,
        t.age,
        t.rate,
        t.userType,
        t.sex,
        t.birthday,
        t.education,
        t.bought))   
      )

  }

  val accounts = TableQuery[Accounts]

  class Posts(tag: Tag) extends Table[DBPost](tag, "posts") {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def ownerId = column[Long]("owner_id")
    def productId = column[Option[Long]]("product_id")
    def title = column[String]("title")
    def thumbnail = column[Option[String]]("thumbnail")
    def content = column[String]("content")
    def contentType = column[Int]("content_type")
    def postType = column[Int]("post_type")
    def balance = column[Option[Long]]("balance")
    def limit = column[Option[Long]]("limit")
    def status = column[Int]("status")
    def promo = column[Long]("promo")
    def alcohol = column[Option[Int]]("alcohol")
    def value = column[Option[Int]]("value")
    def address = column[Option[String]]("address")
    def typeStatus = column[Int]("type_status")
    def likesCount = column[Int]("likes_count")
    def commentsCount = column[Int]("comments_count")
    def reviewsCount = column[Int]("reviews_count")
    def created = column[Long]("created")
    def viewsCount = column[Int]("views_count")
    def rewardType = column[Int]("reward_type")
    def rewardToken = column[Long]("reward_token")
    def rewardPower = column[Long]("reward_power")
    def rewardDollar = column[Long]("reward_dollar")
    def rate = column[Int]("rate")
    def rateCount = column[Int]("rate_count")
    def moderateStatus = column[Int]("moderate_status")
    def * = ((
      id,
      ownerId,
      productId,
      title,
      thumbnail,
      content,
      contentType,
      postType,
      balance,
      limit,
      status,
      promo,
      alcohol),(
      value,
      address,
      typeStatus,
      likesCount,
      commentsCount,
      reviewsCount,
      created,
      viewsCount,
      rewardType,
      rewardToken,
      rewardPower,
      rewardDollar,
      rate,
      rateCount,
      moderateStatus)) <> [DBPost](t => DBPost(
          t._1._1,
          t._1._2,
          t._1._3,
          t._1._4,
          t._1._5,
          t._1._6,
          t._1._7,
          t._1._8,
          t._1._9,
          t._1._10,
          t._1._11,
          t._1._12,
          t._1._13,
          t._2._1,
          t._2._2,
          t._2._3,
          t._2._4,
          t._2._5,
          t._2._6,
          t._2._7,
          t._2._8,
          t._2._9,
          t._2._10,
          t._2._11,
          t._2._12,
          t._2._13,
          t._2._14,
          t._2._15
          ), t => Some(
      (t.id,
      t.ownerId,
      t.productId,
      t.title,
      t.thumbnail,
      t.content,
      t.contentType,
      t.postType,
      t.balance,
      t.limit,
      t.status,
      t.promo,
      t.alcohol),(
      t.value,
      t.address,
      t.typeStatus,
      t.likesCount,
      t.commentsCount,
      t.reviewsCount,
      t.created,
      t.viewsCount,
      t.rewardType,
      t.rewardToken,
      t.rewardPower,
      t.rewardDollar,
      t.rate,
      t.rateCount,
      t.moderateStatus)))
  }

  val posts = TableQuery[Posts]

  class Likes(tag: Tag) extends Table[DBLike](tag, "likes") {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def ownerId = column[Long]("owner_id")
    def targetType = column[Int]("target_type")
    def targetId = column[Long]("target_id")
    def created = column[Long]("created")
    def * = (
      id,
      ownerId,
      targetType,
      targetId,
      created) <> (DBLike.tupled, DBLike.unapply)
  }

  val likes = TableQuery[Likes]

  class Transactions(tag: Tag) extends Table[DBTransaction](tag, "txs") {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def created = column[Long]("created")
    def scheduled = column[Option[Long]]("scheduled")
    def processed = column[Option[Long]]("processed")
    def fromType = column[Int]("from_type")
    def toType = column[Int]("to_type")
    def fromId = column[Option[Long]]("from_id")
    def toId = column[Option[Long]]("to_id")
    def fromRouteType = column[Option[Int]]("from_route_type")
    def toRouteType = column[Option[Int]]("to_route_type")
    def fromRouteId = column[Option[Long]]("from_route_id")
    def toRouteId = column[Option[Long]]("to_route_id")
    def from = column[Option[String]]("from")
    def to = column[Option[String]]("to")
    def txType = column[Int]("type")
    def msg = column[Option[String]]("msg")
    def state = column[Int]("state")
    def currencyId = column[Int]("currency_id")
    def amount = column[Long]("amount")
    def * = (
      id,
      created,
      scheduled,
      processed,
      fromType,
      toType,
      fromId,
      toId,
      fromRouteType,
      toRouteType,
      fromRouteId,
      toRouteId,
      from,
      to,
      txType,
      msg,
      state,
      currencyId,
      amount) <> (DBTransaction.tupled, DBTransaction.unapply)
  }

  val transactions = TableQuery[Transactions]
  
  class Batches(tag: Tag) extends Table[DBBatch](tag, "batches") {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def productId = column[Long]("product_id")
    def created = column[Long]("created")
    def count = column[Int]("count")
    def price = column[Long]("price")
    def * = (
        id, 
        productId, 
        created,
        count,
        price) <> (DBBatch.tupled, DBBatch.unapply)
  }

  val batches = TableQuery[Batches]
  
  class Items(tag: Tag) extends Table[DBItem](tag, "items") {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def batchId = column[Long]("batch_id")
    def code = column[String]("code")
    def status = column[Int]("status")
    def bought = column[Option[Long]]("bought")
    def buyerId = column[Option[Long]]("buyer_id")
    def * = (
        id, 
        batchId, 
        code, 
        status,
        bought,
        buyerId) <> (DBItem.tupled, DBItem.unapply)
  }

  val items = TableQuery[Items]

  class MarketingCampaigns(tag: Tag) extends Table[DBMarketingCampaign](tag, "marketing_campaigns") {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def productId = column[Long]("product_id")
    def initialCount = column[Int]("initial_count")
    def count = column[Int]("count")
    def price = column[Long]("price")
    def start = column[Long]("start")
    def end = column[Long]("end")
    def status = column[Int]("status")
    def descr = column[Option[String]]("descr")
    def title = column[String]("title")
    def * = (
        id, 
        productId, 
        initialCount, 
        count,
        price,
        start,
        end,
        status,
        descr,
        title) <> (DBMarketingCampaign.tupled, DBMarketingCampaign.unapply)
  }

  val marketingCampaigns = TableQuery[MarketingCampaigns]

    class ShortOptions(tag: Tag) extends Table[DBShortOption](tag, "short_options") {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def name = column[String]("name")
    def descr = column[String]("descr")
    def ttype = column[String]("type")
    def value = column[String]("value")
    def * = (
        id, 
        name,
        descr,
        ttype,
        value) <> (DBShortOption.tupled, DBShortOption.unapply)
  }

  val shortOptions = TableQuery[ShortOptions]
  
}

