package services

import com.typesafe.config.Config

import javax.inject.Inject
import javax.inject.Singleton
import net.sargue.mailgun.Configuration
import net.sargue.mailgun.Mail

trait Mailer {

  def sendVerificationToken(emailPatternName: String, to: String, login: String, code: String): MailerResponse

  def send(to: String, subject: String, content: String): MailerResponse

}

case class MailerResponse(val isOk: Boolean, val status: Int, val msg: String)

@Singleton
class MailGunMailer @Inject() (config: Config) extends Mailer {

  val configuration = new Configuration()
    .domain(config.getString("mailer.domain"))
    .apiKey(config.getString("mailer.apikey"))
    .from(config.getString("mailer.fromname"), config.getString("mailer.from"))

  val subject = config.getString("mailer.subject")

  override def sendVerificationToken(emailPattern: String, to: String, login: String, code: String): MailerResponse =
    send(to, subject, config.getString(emailPattern)
      .replace("%account.login%", login)
      .replace("%account.confirmCode%", code))

  override def send(to: String, subject: String, content: String): MailerResponse = {
    val response = Mail.using(configuration)
      .to(to)
      .subject(subject)
      .text(content)
      .build()
      .send()
    MailerResponse(response.isOk(), response.responseCode(), response.responseMessage())
  }

}
