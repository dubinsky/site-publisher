package org.podval.tools.publish.site

import org.podval.tools.publish.page.{DirectoryPage, MarkupPage, Page}
import org.podval.tools.publish.util.Json
import org.podval.xml.Xml
import org.podval.xml.dsl.{*, given}

object Seo:
  val generatorName: String = "Podval Site Publisher"
  val generatorUrl: String = "https://github.com/dubinsky/site-publisher"

  def schemaType(page: Page): String =
    if isHome(page) then "WebSite"
    else if page.date.isDefined || page.isPost
    then "BlogPosting"
    else "WebPage"

  def head(page: MarkupPage): Seq[Xml.Element] =
    val site: Site = page.site
    val pageTitle: String = page.title
    val desc: String = description(page)
    val url: String = canonical(page)
    val authorName: String = author(page)
    val published: Option[String] = page.publishedAt.map(_.toString)
    val modified: Option[String] = page.updatedAt.map(_.toString)
    val isArticle: Boolean = page.date.isDefined

    List(
      Some(title(documentTitle(page))),
      Some(meta(name := "generator", contentAttr := s"$generatorName ($generatorUrl)")),
      Some(meta(attr("property") := "og:title", contentAttr := pageTitle)),
      Some(meta(name := "author", contentAttr := authorName)),
      Some(meta(attr("property") := "og:locale", contentAttr := ogLocale(page.lang))),
      Some(meta(name := "description", contentAttr := desc)),
      Some(meta(attr("property") := "og:description", contentAttr := desc)),
      Some(meta(name := "twitter:description", contentAttr := desc)),
      Some(link(rel := "canonical", href := url)),
      Some(meta(attr("property") := "og:url", contentAttr := url)),
      Some(meta(attr("property") := "og:site_name", contentAttr := site.config.title)),
      Some(meta(attr("property") := "og:type", contentAttr := (if isArticle then "article" else "website"))),
      published.map(t => meta(attr("property") := "article:published_time", contentAttr := t)),
      Option.when(isArticle)(modified).flatten.map(t =>
        meta(attr("property") := "article:modified_time", contentAttr := t)
      ),
      Some(meta(name := "twitter:card", contentAttr := "summary")),
      Some(meta(name := "twitter:title", contentAttr := pageTitle)),
      site.config.social.twitter.map(handle =>
        meta(name := "twitter:site", contentAttr := s"@${handle.stripPrefix("@")}")
      ),
      Some(script(typeAttr := "application/ld+json", jsonLd(
        page = page,
        url = url,
        desc = desc,
        authorName = authorName,
        published = published,
        modified = modified
      )))
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

  private def canonical(page: Page): String = s"${page.site.uri}${page.publishedPath}"

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
      Some("@context" -> Json.string("https://schema.org")),
      Some("@type" -> Json.string(kind)),
      Some("url" -> Json.string(url)),
      Some("name" -> Json.string(page.title)),
      Some("headline" -> Json.string(page.title)),
      Some("description" -> Json.string(desc)),
      Some("author" -> obj("@type" -> Json.string("Person"), "name" -> Json.string(authorName))),
      Some("publisher" -> obj(
        "@type" -> Json.string("Organization"),
        "name" -> Json.string(page.site.config.title)
      )),
      published.map("datePublished" -> Json.string(_)),
      modified.map("dateModified" -> Json.string(_)),
      Option.when(kind == "BlogPosting")(
        "mainEntityOfPage" -> obj("@type" -> Json.string("WebPage"), "@id" -> Json.string(url))
      )
    ).flatten
    obj(fields *)

  private def obj(fields: (String, String)*): String =
    fields.map((key, value) => s"${Json.string(key)}:$value").mkString("{", ",", "}")
