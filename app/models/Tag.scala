package models

import java.util.Arrays

import org.commonmark.ext.gfm.tables.TablesExtension
//import laika.api.Transform
import org.commonmark.parser.Parser
import org.commonmark.renderer.html.HtmlRenderer

//import com.vladsch.flexmark.ast.Node
//import com.vladsch.flexmark.ext.gfm.strikethrough.StrikethroughExtension
//import com.vladsch.flexmark.ext.tables.TablesExtension
//import com.vladsch.flexmark.ext.abbreviation.AbbreviationExtension
//import com.vladsch.flexmark.ext.admonition.AdmonitionExtension
//import com.vladsch.flexmark.ext.anchorlink.AnchorLinkExtension
//import com.vladsch.flexmark.ext.aside.AsideExtension
//import com.vladsch.flexmark.ext.autolink.AutolinkExtension
//import com.vladsch.flexmark.ext.emoji.EmojiExtension
//import com.vladsch.flexmark.ext.typographic.TypographicExtension
//import com.vladsch.flexmark.ext.footnotes.FootnoteExtension
//
//import com.vladsch.flexmark.html.HtmlRenderer
//import com.vladsch.flexmark.parser.Parser
//import com.vladsch.flexmark.util.options.MutableDataSet
//import com.vladsch.flexmark.util.options.MutableDataSet

import play.api.libs.json.JsObject
//import laika.parse.markdown.Markdown
import play.api.libs.json.JsValue
import play.api.libs.json.Json
import play.api.libs.json.JsArray
import sun.java2d.pipe.TextRenderer
import org.jsoup.Jsoup
import controllers.AppConstants
import controllers.AppContext

class Tag(
  val id:   Long,
  val name: String) extends TraitDateSupports {

  def toCommonJs(implicit ac: AppContext): JsObject = {
    var jsObj = Json.obj(
      "id" -> id,
      "name" -> name)
    jsObj
  }

  def toJson(implicit ac: AppContext): JsValue = toCommonJs

}
