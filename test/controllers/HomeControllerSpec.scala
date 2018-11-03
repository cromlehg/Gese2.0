//package controllers
//
//import org.scalatestplus.play._
//import org.scalatestplus.play.guice._
//import play.api.test._
//import play.api.test.Helpers._
//import models.daos.DAO
////import play.api.inject.guice.GuiceApplicationBuilder
//import org.scalatest.TestData
//import play.api.Application
//import play.api.Mode
//import com.typesafe.config.Config
//import play.api.inject.Injector
//import play.api.db.slick.DatabaseConfigProvider
//import scala.concurrent.ExecutionContext.Implicits.global
//
///**
// * Add your spec here.
// * You can mock out a whole application including requests, plugins etc.
// *
// * For more information, see https://www.playframework.com/documentation/latest/ScalaTestingWithScalaTest
// */
//class HomeControllerSpec extends PlaySpec with GuiceOneAppPerTest with Injecting {
////
////  //  lazy val appBuilder: GuiceApplicationBuilder = new GuiceApplicationBuilder().in(Mode.Test)
////  //  lazy val injector: Injector = appBuilder.injector()
////  //  lazy val dbConfProvider: DatabaseConfigProvider = injector.instanceOf[DatabaseConfigProvider]
////
////  implicit override def newAppForTest(testData: TestData): Application = {
////    new GuiceApplicationBuilder().configure(Map(
////      "slick.dbs.default.driver" -> "slick.driver.H2Driver$",
////      "slick.dbs.default.db.driver" -> "org.h2.Driver",
////      "slick.dbs.default.db.url" -> "jdbc:h2:mem:play")).build()
////  }
////
////  "HomeController GET" should {
////
////    "render the index page from a new instance of controller" in {
////      val dao = Application.instanceCache[DAO].apply(app)
////
////      val controller = new AppController(stubControllerComponents(), dao)
////      val home = controller.index().apply(FakeRequest(GET, "/"))
////
////      status(home) mustBe OK
////      contentType(home) mustBe Some("text/html")
////      contentAsString(home) must include("Dream Real Backend Server v0.1 alpha")
////    }
////
////    "render the index page from the application" in {
////      val controller = inject[AppController]
////      val home = controller.index().apply(FakeRequest(GET, "/"))
////
////      status(home) mustBe OK
////      contentType(home) mustBe Some("text/html")
////      contentAsString(home) must include("Dream Real Backend Server v0.1 alpha")
////    }
////
////    "render the index page from the router" in {
////      val request = FakeRequest(GET, "/")
////      val home = route(app, request).get
////
////      status(home) mustBe OK
////      contentType(home) mustBe Some("text/html")
////      contentAsString(home) must include("Dream Real Backend Server v0.1 alpha")
////    }
////  }
//}
