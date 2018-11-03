package controllers

object AppConstants {

  val APP_NAME = "Gese"

  val BACKEND_NAME = "Backend Core v0.1"

  val PWD_MIN_LENGTH = 10

  val DECIMALS = 100

  val ACTIVE_CAMPAINGS_LIMIT = 5

  /**
   *
   * Механизм инфляции.
   *
   * На платформе происходит постоянный выпуск DOLLARS и POWER.
   * При этом мы можем купить за них TOKENS. Но покупка происходит не сразу,
   * а через n дней. Однако это прямая безконтролная эмиссия, если брать начальный курс.
   * Поэтому курс будет меняться в зависимости от спроса на TOKEN???
   *
   * Транзакции на покупку TOKEN будут обрабатываться в порядке очереди инфляции по стабильному курсу.
   * И в зависимости от спроса и наличия токенов на балансе системы курс будет меняться.
   *
   * Транзакции конвертации в токены будут обрабатываться следующим образом:
   * - токенов даваться будет не более чем установленая цена изначально
   * - актуальная цена будет браться из расчета что:
   *   на балансе резерв 70%
   *   30%
   *
   */

  //  val INITIAL_POOL_VALUE = 1000 * DECIMALS
  //
  //  val INITIAL_POOL_INFLATION: Double = 9.1
  //
  //  val END_POOL_INFLATION: Double = 0.1
  //
  //  val POOL_DAY_INFLATION_DECREASE: Double = 0.005

  val INITIAL_DP = 5 * DECIMALS

  val SESSION_EXPIRE_TYME: Long = 3 * TimeConstants.DAY

  val POSTS_COUNTER_LIMIT = 10

  val POSTS_COUNTER_TIME_LIMIT: Long = TimeConstants.DAY

  val COMMENTS_COUNTER_LIMIT = 50

  val COMMENTS_COUNTER_TIME_LIMIT: Long = TimeConstants.DAY

  val LIKES_COUNTER_LIMIT = 20

  val LIKES_COUNTER_TIME_LIMIT: Long = TimeConstants.DAY

  val MIN_TITLE_LENGTH = 3

  val MIN_CONTENT_LENGTH = 255

  val DOLLAR_TO_TOKEN_CHANGE_INTERVAL: Long = 5 * TimeConstants.DAY

  val POWER_TO_TOKEN_CHANGE_INTERVAL: Long = 100 * TimeConstants.DAY

  val DESCRIPTION_SIZE = 300

  val TAGS_PER_POST_LIMIT = 4

  val TAG_SIZE_LIMIT = 3

  val INTERVAL_TO_REWARDER_TASK: Long = TimeConstants.DAY

  val INTERVAL_TO_SCHEDULED_TXS_PROCESSOR_TASK: Long = TimeConstants.HOUR

  val INTERVAL_NOTIFICATIONS_TASK: Long = 20 * TimeConstants.MINUTE

  val NOTIFICATION_INTERVAL: Long = 2 * TimeConstants.HOUR

}
