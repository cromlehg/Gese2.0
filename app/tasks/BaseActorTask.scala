package tasks

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.DurationInt

import akka.actor.ActorSystem
import javax.inject.Inject
import models.daos.DAO
import controllers.RewardLogic
import controllers.AppConstants
import play.Logger
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import models.ScheduledTask
import models.TaskType
import scala.concurrent.Future

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

import services.Mailer
import services.MailerResponse
import com.typesafe.config.Config

import javax.inject.Inject
import javax.inject.Singleton
import models.daos.DAO
import play.Logger
import play.api.mvc.ControllerComponents
import play.api.mvc.Result
import java.util.regex.Pattern
import java.io.IOException

import scala.util.{ Try, Success, Failure }
import scala.concurrent.{ Await, Future }
import scala.concurrent.duration.Duration

class BaseActorTask @Inject() (mailer: Mailer, actorSystem: ActorSystem, val dao: DAO, config: Config)(implicit executionContext: ExecutionContext) {

  actorSystem.scheduler.schedule(initialDelay = 1.minutes, interval = AppConstants.INTERVAL_NOTIFICATIONS_TASK.toInt.milliseconds) {
    val start = System.currentTimeMillis
    Logger.debug("Start to send notifications")
    val r =
      dao.getNotificationsPages() flatMap { count =>
        Logger.debug("Notification pages " + count);
        Future.sequence(for (i <- 1 to count.toInt) yield dao.getNotificationTasksWithEmailAndProduct(i) flatMap { tasks =>
          tasks foreach { task =>

            try {
              mailer.send(
                task.emailOpt.get,
                config getString "mailer.subject2",
                (config getString "mailer.pattern2").replace("%product.id%", task.productId.get.toString)) match {
                  case MailerResponse(true, _, _) =>
                    Logger.debug("Notification successfully sended to " + task.emailOpt.get)

                  case MailerResponse(false, status, msg) =>
                    Logger.error("can't send email")
                    Logger.error("status code: " + status)
                    Logger.error("message: " + msg)
                }

            } catch {
              case e: IOException =>
                Logger.error(e.toString())
            }

          }
          dao.notificationsSended(tasks.map(_.id))
        }).map(_.sum)
      } map { r =>
        Logger.debug("Sending notifications successfully finished.")
        r
      }

    Await.ready(r, Duration.Inf).onComplete {
      case Success(t) => Logger.debug("Sending notifications success!")
      case Failure(e) => Logger.debug("Sending notifications failed!" + e.getMessage + " \n " + e.printStackTrace())
    }

    Logger.debug("Sending notifications task end - " + (System.currentTimeMillis - start) + " ms")
  }

  actorSystem.scheduler.schedule(initialDelay = 5000.microseconds, interval = AppConstants.INTERVAL_TO_REWARDER_TASK.toInt.milliseconds) {
    val pageSize = 2000
    val start = System.currentTimeMillis
    Logger.debug("Start to update balances")
    val r = dao.getTaskLastExecution(TaskType.REWARDER) flatMap { lastTaskExecution =>
      if (System.currentTimeMillis >= lastTaskExecution + AppConstants.INTERVAL_TO_REWARDER_TASK) {
        Logger.debug("Process reset counters")
        dao.getAccountsPages(pageSize) flatMap { count =>
          Future.sequence(for (i <- 1 to count.toInt) yield dao.resetCounters(pageSize, i)).map(_.sum)
        } flatMap { r =>
          Logger.debug("Reset counters successfully finished for " + r + " . Start to update balances...")
          dao.getAccountBalancesPages(pageSize) flatMap { countBalances =>
            Logger.debug("Update balances pages " + countBalances)
            Future {
              (1 to countBalances.toInt).map { pageId =>
                Logger.debug("Update balances page " + pageId)
                val startInner = System.currentTimeMillis
                val inR = Await.result(dao.updateBalances(pageSize, pageId), Duration.Inf)
                Logger.debug("Update balances for page " + pageId + " finished during " + (System.currentTimeMillis - startInner) + " ms")
                inR
              }.sum
            }
          } flatMap { r =>
            Logger.debug("Update balances successfully finished for " + r)
            dao.updateTaskExecutionTime(TaskType.REWARDER)
          } map { r =>
            if (r)
              Logger.debug("Update rewarder task time successfully finished")
            else
              Logger.error("Update rewarder task failed")
            r
          }
        }
      } else {
        Logger.debug("Update balances not started because interval not reached")
        Future.successful(true)
      }
    }

    Await.ready(r, Duration.Inf).onComplete {
      case Success(t) => Logger.debug("Update balances success!")
      case Failure(e) => Logger.debug("Update balances failed!")
    }

    Logger.debug("Update balances task end - " + (System.currentTimeMillis - start) + " ms")

  }

  actorSystem.scheduler.schedule(initialDelay = 2.minutes, interval = AppConstants.INTERVAL_TO_SCHEDULED_TXS_PROCESSOR_TASK.toInt.milliseconds) {
    val start = System.currentTimeMillis
    Logger.debug("Start to approve scheduled transactions")
    val r = dao.getTaskLastExecution(TaskType.SCHEDULED_TXS_PROCESSOR) flatMap { lastTaskExecution =>
      if (System.currentTimeMillis >= lastTaskExecution + AppConstants.INTERVAL_TO_SCHEDULED_TXS_PROCESSOR_TASK) {
        Logger.debug("Process approve scheduled transactions")
        dao.approveScheduledTransactions() flatMap { length =>
          Logger.debug("Approve successfully finnished. Approved " + length)
          dao.updateTaskExecutionTime(TaskType.SCHEDULED_TXS_PROCESSOR)
        } map { r =>
          Logger.debug("Update txs processor time successfully finished")
          r
        }
      } else {
        Logger.debug("Txs processor task not started becasue interval not reached")
        Future.successful(true)
      }
    }

    Await.ready(r, Duration.Inf).onComplete {
      case Success(t) => Logger.debug("Approve scheduled txs success!")
      case Failure(e) => Logger.debug("Approve scheduled txs failed!")
    }

    Logger.debug("Approve scheduled txs task end - " + (System.currentTimeMillis - start) + " ms")

  }

}