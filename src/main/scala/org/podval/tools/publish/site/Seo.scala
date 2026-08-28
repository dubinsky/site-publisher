package org.podval.tools.publish.site

import org.podval.tools.publish.page.{DirectoryPage, MarkupPage, Page}
import org.podval.tools.publish.util.Date
import org.podval.xml.Html
import zio.blocks.html.{Js, content as contentAttribute, title as titleElement, *}
import java.time.{Instant, LocalTime, ZoneId}
import scala.util.Try

object Seo:
  val generatorName: String = "Podval Site Publisher"
  val generatorUrl: String = "https://github.com/dubinsky/site-publisher"

  def schemaType(page: Page): String =
    if isHome(page) then "WebSite"
    else if page.date.isDefined || page.isPost
    then "BlogPosting"
    else "WebPage"

  def head(page: MarkupPage): Seq[Html.Element] =
    val site: Site = page.site
    val pageTitle: String = page.title
    val desc: String = description(page)
    val url: String = canonical(page)
    val authorName: String = author(page)
    val published: Option[String] = page.date.map(formatDate(_, site))
    val modified: Option[String] =
      page.dateModified.map(formatDate(_, site))
        .orElse(page.dateModifiedGit.map(formatInstant(_, site)))
        .orElse(published)
    val isArticle: Boolean = page.date.isDefined

    List(
      Some(titleElement(documentTitle(page))),
      Some(meta(name := "generator", contentAttribute := s"$generatorName ($generatorUrl)")),
      Some(meta(attr("property") := "og:title", contentAttribute := pageTitle)),
      Some(meta(name := "author", contentAttribute := authorName)),
      Some(meta(attr("property") := "og:locale", contentAttribute := ogLocale(page.lang))),
      Some(meta(name := "description", contentAttribute := desc)),
      Some(meta(attr("property") := "og:description", contentAttribute := desc)),
      Some(meta(name := "twitter:description", contentAttribute := desc)),
      Some(link(rel := "canonical", href := url)),
      Some(meta(attr("property") := "og:url", contentAttribute := url)),
      Some(meta(attr("property") := "og:site_name", contentAttribute := site.config.title)),
      Some(meta(attr("property") := "og:type", contentAttribute := (if isArticle then "article" else "website"))),
      published.map(t => meta(attr("property") := "article:published_time", contentAttribute := t)),
      Option.when(isArticle)(modified).flatten.map(t =>
        meta(attr("property") := "article:modified_time", contentAttribute := t)
      ),
      Some(meta(name := "twitter:card", contentAttribute := "summary")),
      Some(meta(name := "twitter:title", contentAttribute := pageTitle)),
      site.config.social.twitter.map(handle =>
        meta(name := "twitter:site", contentAttribute := s"@${handle.stripPrefix("@")}")
      ),
      Some(script(`type` := "application/ld+json", Js(jsonLd(
        page = page,
        url = url,
        desc = desc,
        authorName = authorName,
        published = published,
        modified = modified
      ))))
    ).flatten

  private def isHome(page: Page): Boolean = page.path.path == Seq(DirectoryPage.fileName)

  private def documentTitle(page: Page): String =
    val siteTitle: String = page.site.config.title
    if isHome(page) || page.title == siteTitle then siteTitle
    else s"${page.title} | $siteTitle"

  private def description(page: Page): String =
    page.description.getOrElse(page.site.config.description)

  private def author(page: MarkupPage): String =
    page.asFullMarkupPage.flatMap(_.author).getOrElse(page.site.config.author)

  private def canonical(page: Page): String = s"${page.site.uri}${page.site.pages.publishedPath(page)}"

  private def ogLocale(lang: String): String =
    val normalized: String = lang.replace('-', '_')
    if normalized.contains('_') then normalized
    else if normalized.equalsIgnoreCase("en") then "en_US"
    else normalized

  private def jsonLd(
    page: MarkupPage,
    url: String,
    desc: String,
    authorName: String,
    published: Option[String],
    modified: Option[String]
  ): String =
    val kind: String = schemaType(page)
    val fields: List[(String, String)] = List(
      Some("@context" -> jsonStr("https://schema.org")),
      Some("@type" -> jsonStr(kind)),
      Some("url" -> jsonStr(url)),
      Some("name" -> jsonStr(page.title)),
      Some("headline" -> jsonStr(page.title)),
      Some("description" -> jsonStr(desc)),
      Some("author" -> obj("@type" -> jsonStr("Person"), "name" -> jsonStr(authorName))),
      Some("publisher" -> obj(
        "@type" -> jsonStr("Organization"),
        "name" -> jsonStr(page.site.config.title)
      )),
      published.map("datePublished" -> jsonStr(_)),
      modified.map("dateModified" -> jsonStr(_)),
      Option.when(kind == "BlogPosting")(
        "mainEntityOfPage" -> obj("@type" -> jsonStr("WebPage"), "@id" -> jsonStr(url))
      )
    ).flatten
    obj(fields *)

  private def obj(fields: (String, String)*): String =
    fields.map((key, value) => s"${jsonStr(key)}:$value").mkString("{", ",", "}")

  private def jsonStr(s: String): String =
    val escaped: String = s.flatMap:
      case '"' => "\\\""
      case '\\' => "\\\\"
      case '\b' => "\\b"
      case '\f' => "\\f"
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case '<' => "\\u003c"
      case '>' => "\\u003e"
      case '&' => "\\u0026"
      case c if c < 32 => f"\\u${c.toInt}%04x"
      case c => c.toString
    s"\"$escaped\""

  private def zone(site: Site): ZoneId =
    site.config.timezone.flatMap(tz => Try(ZoneId.of(tz)).toOption).getOrElse(ZoneId.systemDefault)

  private def formatDate(date: Date, site: Site): String = date match
    case date: Date.OffsetTime => date.value.toString
    case date: Date.LocalTime => date.value.atZone(zone(site)).toOffsetDateTime.toString
    case date: Date.Local => date.value.atTime(LocalTime.MIDNIGHT).atZone(zone(site)).toOffsetDateTime.toString

  private def formatInstant(instant: Instant, site: Site): String =
    instant.atZone(zone(site)).toOffsetDateTime.toString
