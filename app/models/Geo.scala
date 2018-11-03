package models

case class Geo(val longitude: Double, val latitude: Double, val accuracy: Double)

object Geo {

  import play.api.libs.functional.syntax._
  import play.api.libs.json._

  implicit val geoReads: Reads[Geo] = (
    (JsPath \ "longitude").read[Double] and
    (JsPath \ "latitude").read[Double] and
    (JsPath \ "accuracy").read[Double])(Geo.apply _)

}

