package models;

import java.text.SimpleDateFormat
import java.util.Date

trait TraitDateSupports {

  val simpleShortFormatter = new SimpleDateFormat("yyyy/MM/dd")

  def formattedShortDate(date: Long) = simpleShortFormatter.format(new Date(date))

}