package controllers

import scala.collection.mutable.Map

class AppContext() {

  var authorizedOpt: Option[models.Account] = None

  var props: Map[String, Any] = Map()

  def propBool(name: String): Boolean = props.get(name).map(_.asInstanceOf[Boolean]).getOrElse(false)

}