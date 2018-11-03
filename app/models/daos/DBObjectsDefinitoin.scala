package models.daos

case class DBWallet(
  id: Long,
  owner_id: Option[Long],
  owner_type_id: Int,
  base_currency_id: Int,
  address: String,
  private_key: String)

case class DBCurrency(
  val id: Int,
  val ticker: String,
  val name: String)

case class DBBalance(
  val id: Long,
  val ownerId: Long,
  val ownerType: Int,
  val currencyId: Int,
  val updated: Long,
  val balanceType: Int,
  val value: Long)

case class DBPosition(
  val id: Long,
  val itemId: Long,
  val timestamp: Long,
  val longitude: Double,
  val latitude: Double,
  val accuracy: Double)

case class DBRole(
  userId: Long,
  role: Int)

case class DBScheduledTask(
  val id: Long,
  val executed: Option[Long],
  val taskType: Int,
  val planned: Option[Long],
  val accountId: Option[Long],
  val productId: Option[Long])

case class DBTag(
  id: Long,
  name: String)

case class DBTagToTarget(
  tagId: Long,
  targetId: Long,
  targetType: Int,
  created: Long)

case class DBComment(
  val id: Long,
  val postId: Long,
  val ownerId: Long,
  val parentId: Option[Long],
  val content: String,
  val contentType: Int,
  val created: Long,
  val likesCount: Int,
  val rewardToken: Long,
  val rewardPower: Long,
  val rewardDollar: Long,
  val status: Int)

case class DBSession(
  val id: Long,
  val userId: Long,
  val ip: String,
  val sessionKey: String,
  val created: Long,
  val expire: Long)

case class DBAccount(
  val id: Long,
  val login: String,
  val email: String,
  val hash: Option[String],
  val avatar: Option[String],
  val background: Option[String],
  val userStatus: Int,
  val accountStatus: Int,
  val name: Option[String],
  val surname: Option[String],
  val platformEth: Option[String],
  val timezoneId: Int,
  val registered: Long,
  val confirmCode: Option[String],
  val postsCounter: Int,
  val postsCounterStarted: Long,
  val likesCounter: Int,
  val likesCounterStarted: Long,
  val commentsCounter: Int,
  val commentsCounterStarted: Long,
  val postsCount: Long,
  val about: Option[String],
  val isShowBrand: Boolean,
  val ITN: Option[String],
  val IEC: Option[String],
  val phones: Option[String],
  val gender: Option[String],
  val age: Option[Int],
  val rate: Int,
  val userType: Int,
  val sex: Option[Int],
  val birthday: Option[Long],
  val education: Option[String],
  val bought: Int)

case class DBPost(
  val id: Long,
  val ownerId: Long,
  val productId: Option[Long],
  val title: String,
  val thumbnail: Option[String],
  val content: String,
  val contentType: Int,
  val postType: Int,
  val balance: Option[Long],
  val limit: Option[Long],
  val status: Int,
  val promo: Long,
  val alcohol: Option[Int],
  val value: Option[Int],
  val address: Option[String],
  val typeStatus: Int,
  val likesCount: Int,
  val commentsCount: Int,
  val reviewsCount: Int,
  val created: Long,
  val viewsCount: Int,
  val rewardType: Int,
  val rewardToken: Long,
  val rewardPower: Long,
  val rewardDollar: Long,
  val rate: Int,
  val rateCount: Int,
  val moderateStatus: Int)

case class DBLike(
  val id: Long,
  val ownerId: Long,
  val targetType: Int,
  val targetId: Long,
  val created: Long)

case class DBTransaction(
  val id: Long,
  val created: Long,
  val scheduled: Option[Long],
  val processed: Option[Long],
  val fromType: Int,
  val toType: Int,
  val fromId: Option[Long],
  val toId: Option[Long],
  val fromRouteType: Option[Int],
  val toRouteType: Option[Int],
  val fromRouteId: Option[Long],
  val toRouteId: Option[Long],
  val from: Option[String],
  val to: Option[String],
  val txType: Int,
  val msg: Option[String],
  val state: Int,
  val currencyId: Int,
  val amount: Long)

case class DBProduct(
  val id: Long,
  val ownerId: Long,
  val name: String,
  val alcohol: Int,
  val value: Int,
  val address: Option[String],
  val about: Option[String],
  val aboutType: Option[Int],
  val created: Long,
  val reviewsCount: Int,
  val likesCount: Int,
  val commentsCount: Int,
  val thumbnail: Option[String])

case class DBBatch(
  val id: Long,
  val productId: Long,
  val created: Long,
  val count: Int,
  val price: Long)

case class DBItem(
  val id: Long,
  val batchId: Long,
  val code: String,
  val status: Int,
  val bought: Option[Long],
  val buyerId: Option[Long])

case class DBMarketingCampaign(
  val id: Long,
  val productId: Long,
  val initialCount: Int,
  val count: Int,
  val price: Long,
  val start: Long,
  val end: Long,
  val status: Int,
  val descr: Option[String],
  val title: String)

case class DBShortOption(
  val id: Long,
  val name: String,
  val descr: String,
  val ttype: String,
  val value: String)
