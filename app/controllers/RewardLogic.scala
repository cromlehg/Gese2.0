package controllers

import models.RewardType

object RewardLogic {

  val POWER_IN_DOLLAR = 10

  val TOKEN_IN_POWER = 10

  val POWER_IN_TOKEN = 0.001

  val DOLLAR_IN_TOKEN = 0.1

  val REWARD_POOL_DIV = 15

  val POST_REWARD_POOL_K = 0.75

  val COMMENT_REWARD_POOL_K = 0.75

  val LIKE_FOR_ACTOR_REWARD_POOL_K = 0.005

  val LIKE_FOR_TARGET_REWARD_POOL_K = 0.75

  def rewardPool(dp: Double) =
    if (dp == 0) 0 else dp / 15

  def POWERInDOLLAR(power: Double): Long = if (power == 0) 0 else (power / POWER_IN_DOLLAR).toLong

  def POWERInTOKEN(power: Double): Long = if (power == 0) 0 else (power * POWER_IN_TOKEN).toLong

  def TOKENInPOWER(token: Double): Long = if (token == 0) 0 else (token * TOKEN_IN_POWER).toLong

  def DOLLARInTOKEN(dollar: Long): Long = if (dollar == 0) 0 else (dollar * DOLLAR_IN_TOKEN).toLong

  def TOKENInDOLLAR(token: Long): Long = token

  def postRewardPool(dp: Double) = (rewardPool(dp) * POST_REWARD_POOL_K).toLong

  def commentRewardPool(dp: Double) = (rewardPool(dp) * COMMENT_REWARD_POOL_K).toLong

  def likeRewardPoolForLiker(dp: Double) = (rewardPool(dp) * LIKE_FOR_ACTOR_REWARD_POOL_K).toLong

  def likeRewardPoolForTarget(dp: Double) = (rewardPool(dp) * LIKE_FOR_TARGET_REWARD_POOL_K).toLong

  def postRewardPower(p: Double, rewardType: Int): Long =
    rewardType match {
      case RewardType.POWER => {
        val r = postRewardPool(p)
        if(r > 1000) 1000 else r
      }
      case RewardType.POWER_50_DOLLARS_50 => {
        val r = (0.5 * postRewardPool(p)).toLong
        if(r > 1000) 1000 else r
      }
    }

  def postRewardDollars(p: Long, rewardType: Int): Long = 
    rewardType match {
      case RewardType.POWER  => 0
      case RewardType.POWER_50_DOLLARS_50 => {
        val r = POWERInDOLLAR(0.5 * postRewardPool(p))
        if(r > 1000) 1000 else r
      }
    }

  def postRewardByLikePower(dp: Double, rewardType: Int): Long =
    rewardType match {
      case RewardType.POWER => {
        val r = likeRewardPoolForTarget(dp)
        if(r > 1000) 1000 else r
      }
      case RewardType.POWER_50_DOLLARS_50 => {
        val r = (0.5 * likeRewardPoolForTarget(dp)).toLong
        if(r > 1000) 1000 else r
      }
    }

  def postRewardByLikeDollars(p: Long, rewardType: Int): Long =
    rewardType match {
      case RewardType.POWER                     => 0
      case RewardType.POWER_50_DOLLARS_50 => {
        val r = POWERInDOLLAR(0.5 * likeRewardPoolForTarget(p))
        if(r > 1000) 1000 else r
      }
    }

  def likerRewardByLikePower(p: Double): Long = {
    val r = likeRewardPoolForLiker(p)
    if(r > 1000) 1000 else r;
  }

  def commentRewardPower(p: Double): Long = {
    val r = commentRewardPool(p)
    if(r > 1000) 1000 else r;
  }

  def rewardDollarEquivalent(rewardDollar: Long, rewardPower: Long): Long =
    (rewardDollar + (rewardPower / RewardLogic.POWER_IN_DOLLAR).toLong)

}
