package models.daos

trait TraitDTOToModel {

  def taskFrom(dto: DBScheduledTask) =
    new models.ScheduledTask(
      dto.id,
      dto.executed,
      dto.taskType,
      dto.planned,
      dto.accountId,
      dto.productId)

  def sessionFrom(dto: DBSession) =
    new models.Session(
      dto.id,
      dto.userId,
      dto.ip,
      dto.sessionKey,
      dto.created,
      dto.expire)

  def batchFrom(dto: DBBatch) =
    new models.Batch(
      dto.id,
      dto.productId,
      dto.created,
      dto.count,
      dto.price)

  def positionFrom(dto: DBPosition) =
    new models.Position(
      dto.id,
      dto.itemId,
      dto.timestamp,
      dto.longitude,
      dto.latitude,
      dto.accuracy)

  def itemFrom(dto: DBItem) =
    new models.Item(
      dto.id,
      dto.batchId,
      dto.code,
      dto.status,
      dto.bought,
      dto.buyerId)

  def likeFrom(dto: DBLike) =
    new models.Like(
      dto.id,
      dto.ownerId,
      dto.targetType,
      dto.targetId,
      dto.created)

  def commentFrom(dto: DBComment) =
    new models.Comment(
      dto.id,
      dto.postId,
      dto.ownerId,
      dto.parentId,
      dto.content,
      dto.contentType,
      dto.created,
      dto.likesCount,
      dto.rewardToken,
      dto.rewardPower,
      dto.rewardDollar,
      dto.status)

  def tagFrom(dto: DBTag) =
    new models.Tag(
      dto.id,
      dto.name)

  def transactionFrom(dto: DBTransaction) =
    new models.Transaction(
      dto.id,
      dto.created,
      dto.scheduled,
      dto.processed,
      dto.fromType,
      dto.toType,
      dto.fromId,
      dto.toId,
      dto.fromRouteType,
      dto.toRouteType,
      dto.fromRouteId,
      dto.toRouteId,
      dto.from,
      dto.to,
      dto.txType,
      dto.msg,
      dto.state,
      dto.currencyId,
      dto.amount)

  def accountFrom(dto: DBAccount): models.Account =
    new models.Account(
      dto.id,
      dto.login,
      dto.email,
      dto.hash,
      dto.avatar,
      dto.background,
      dto.userStatus,
      dto.accountStatus,
      dto.name,
      dto.surname,
      dto.platformEth,
      dto.timezoneId,
      dto.registered,
      dto.confirmCode,
      dto.postsCounter,
      dto.postsCounterStarted,
      dto.likesCounter,
      dto.likesCounterStarted,
      dto.commentsCounter,
      dto.commentsCounterStarted,
      dto.postsCount,
      dto.about,
      dto.isShowBrand,
      dto.ITN,
      dto.IEC,
      dto.phones,
      dto.gender,
      dto.age,
      dto.rate,
      dto.userType,
      dto.sex,
      dto.birthday,
      dto.education,
      dto.bought)

  def accountFrom(dto: DBAccount, roles: Int*): models.Account = {
    val user = accountFrom(dto)
    user.roles = roles
    user
  }

  def postFrom(dto: DBPost) =
    new models.Post(
      dto.id,
      dto.ownerId,
      dto.productId,
      dto.title,
      dto.thumbnail,
      dto.content,
      dto.contentType,
      dto.postType,
      dto.balance,
      dto.limit,
      dto.status,
      dto.promo,
      dto.alcohol,
      dto.value,
      dto.address,
      dto.typeStatus,
      dto.likesCount,
      dto.commentsCount,
      dto.reviewsCount,
      dto.created,
      dto.viewsCount,
      dto.rewardType,
      dto.rewardToken,
      dto.rewardPower,
      dto.rewardDollar,
      dto.rate,
      dto.rateCount,
      dto.moderateStatus)

  def currencyFrom(dto: DBCurrency) =
    new models.Currency(
      dto.id,
      dto.ticker,
      dto.name)

  def balanceFrom(dto: DBBalance) =
    new models.Balance(
      dto.id,
      dto.ownerId,
      dto.ownerType,
      dto.currencyId,
      dto.updated,
      dto.balanceType,
      dto.value)

  def marketingCampaignFrom(dto: DBMarketingCampaign) =
    new models.MarketingCampaign(
      dto.id,
      dto.productId,
      dto.initialCount,
      dto.count,
      dto.price,
      dto.start,
      dto.end,
      dto.status,
      dto.descr,
      dto.title)

  def shortOptionFrom(dto: DBShortOption) =
    new models.ShortOption(
      dto.id,
      dto.name,
      dto.descr,
      dto.ttype,
      dto.value)

}