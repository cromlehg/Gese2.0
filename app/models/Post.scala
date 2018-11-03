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
import org.ocpsoft.prettytime.PrettyTime
import java.util.Date

class Post(
  val id:            Long,
  val ownerId:       Long,
  val productId:     Option[Long],
  val title:         String,
  val thumbnail:     Option[String],
  val content:       String,
  val contentType:   Int,
  val postType:      Int,
  val balance:       Option[Long],
  val limit:         Option[Long],
  val status:        Int,
  val promo:         Long,
  val alcohol:       Option[Int],
  val value:         Option[Int],
  val address:       Option[String],
  val typeStatus:    Int,
  val likesCount:    Int,
  val commentsCount: Int,
  val reviewsCount:  Int,
  val created:       Long,
  val viewsCount:    Int,
  val rewardType:    Int,
  val rewardToken:   Long,
  val rewardPower:   Long,
  val rewardDollar:  Long,
  val rate:          Int,
  val rateCount:     Int,
  val moderateStatus: Int) extends TraitDateSupports {

  def getTrimedTitle(size: Int) = if (title.size > size) title.substring(0, size) + "..." else title

  var ownerOpt: Option[Account] = None

  var titleRefOpt: Option[String] = None

  var likedOpt: Option[Boolean] = None

  var tags: Seq[Tag] = Seq.empty

  var likes: Seq[Like] = Seq.empty

  var reviews: Seq[Post] = Seq.empty

  val pattern = """\[.*?\]\s*\(\s*(.*?)\s*(\".*?\")?\s*\)""".r

  val renderFromMarkdownToHTML =
    ContentCompilerHelper.compile(content)

  val description = {
    val descr = Jsoup.parse(renderFromMarkdownToHTML).text()
    if (descr.size > AppConstants.DESCRIPTION_SIZE) descr.substring(0, AppConstants.DESCRIPTION_SIZE) else descr
  }

  //  def renderFromMarkdownToHTML =
  //    Transform.from(Markdown).to(laika.render.HTML).fromString(content).toString()

  val thumbnailOpt: Option[String] = thumbnail orElse pattern.findFirstMatchIn(content).map(_ group 1)

  val createdShortDate = formattedShortDate(created)

  lazy val reward = new Reward(Some(rewardPower), Some(rewardDollar))

  def rewardsToJson(implicit ac: AppContext) = reward.toJson

  lazy val createdPrettyTime = ContentCompilerHelper.prettyTime.format(new Date(created))

  def toJson()(implicit ac: AppContext): JsObject = {
    var jsObj = Json.obj(
      "id" -> id,
      "owner_id" -> ownerId,
      "title" -> title,
      "post_type" -> PostType.strById(postType),
      "likes_count" -> likesCount,
      "comments_count" -> commentsCount,
      "created" -> created,
      "promo" -> promo,
      "views_count" -> viewsCount,
      "reviews_count" -> reviewsCount,
      "moderate_status" -> ModerateStatus.strById(moderateStatus),
      "reward_type" -> RewardType.strById(rewardType),
      "description" -> description,
      "rate" -> { if (rate == 0 || rateCount == 0) 0 else rate / rateCount }.toInt)
    jsObj = jsObj ++ rewardsToJson
    jsObj = titleRefOpt.fold(jsObj)(t => jsObj ++ Json.obj("product_title" -> t))
    jsObj = alcohol.fold(jsObj)(t => jsObj ++ Json.obj("alcohol" -> t))
    jsObj = productId.fold(jsObj)(t => jsObj ++ Json.obj("product_id" -> t))
    jsObj = value.fold(jsObj)(t => jsObj ++ Json.obj("value" -> t))
    jsObj = address.fold(jsObj)(t => jsObj ++ Json.obj("address" -> t))
    jsObj = likedOpt.fold(jsObj)(liked => jsObj ++ Json.obj("liked" -> liked))
    jsObj = ownerOpt.fold(jsObj)(user => jsObj + ("owner" -> user.toJson))
    jsObj = limit.fold(jsObj) { t => jsObj ++ Json.obj("limit" -> t) }
    jsObj = thumbnailOpt.fold(jsObj) { t => jsObj ++ Json.obj("thumbnail" -> t) }
    jsObj = if (likes.nonEmpty) jsObj + ("likes" -> JsArray(likes.map(_.toJson))) else jsObj
    jsObj = if (tags.nonEmpty) jsObj + ("tags" -> JsArray(tags.map(_.toJson))) else jsObj
    jsObj = balance.fold(jsObj) { t => jsObj ++ Json.obj("balance" -> t) }
    jsObj = if (reviews.nonEmpty) jsObj + ("reviews" -> JsArray(reviews.map(_.toJson))) else jsObj

    if (!ac.propBool("short")) jsObj ++ Json.obj("compiled_content" -> renderFromMarkdownToHTML) else jsObj
  }

}

object ContentCompilerHelper {

  val prettyTime = new PrettyTime()

  //val options = new MutableDataSet()

  // uncomment to set optional extensions
  /*options.set(Parser.EXTENSIONS, Arrays.asList(
      TablesExtension.create(),
      StrikethroughExtension.create(),
      AbbreviationExtension.create(),
      AdmonitionExtension.create(),
      AnchorLinkExtension.create(),
      AsideExtension.create(),
      AutolinkExtension.create(),
      EmojiExtension.create(),
      TypographicExtension.create(),
      FootnoteExtension.create()))*/

  //val parser = Parser.builder(options).build()
  //val renderer = HtmlRenderer.builder(options).build()

  val extensions = Arrays.asList(TablesExtension.create())

  val parser = Parser.builder()
    .extensions(extensions)
    .build()

  val htmlRenderer = HtmlRenderer.builder()
    .extensions(extensions)
    .build()

  def compile(content: String) =
    htmlRenderer.render(parser.parse(content))

}

