import com.google.inject.AbstractModule

import play.api.Configuration
import play.api.Environment
import services.MailGunMailer
import services.Mailer

class Module(
  environment: Environment,
  configuration: Configuration) extends AbstractModule {

  override def configure(): Unit = {
    bind(classOf[Mailer]).to(classOf[MailGunMailer])
  }

}