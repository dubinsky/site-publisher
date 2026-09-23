package org.podval.tools.publish.site

import org.podval.tools.publish.page.{FullMarkupPage, Page, SyntheticMarkupPage}
import org.podval.tools.publish.util.Icon
import org.podval.xml.{Xml, XmlAst}
import org.podval.xml.dsl.{*, given}

final class Tags(site: Site) extends SyntheticMarkupPage(site, Path("tags").html):
  override def titleDefault: String = "Tags"
  override protected def descriptionDefault: Option[String] = Some("Pages by tags")
  override protected def iconDefault: Icon = Icon.tags

  private def fullMarkupPages: List[FullMarkupPage] = site
    .pages
    .pages
    .flatMap(_.asFullMarkupPage)

  private def tagsAll: List[String] = fullMarkupPages
    .flatMap(_.tags)
    .distinct
    .sorted

  private def withTag(tag: String): List[Page] = fullMarkupPages
    .filter(_.tags.contains(tag))
    .sortBy(_.title)

  def tagRef(tag: String): Xml.Element = a(
    className := "page-tag",
    href := s"$path#${XmlAst.toId(tag)}",
    Icon.tag.html,
    tag
  )

  override protected def syntheticContent: Xml.Element =
    div(className := "tags",
      h2("All tags"),
      p(tagsAll.map(tagRef)),
      h2("Pages by tags"),
      ul(tagsAll.map(tag =>
        li(
          h3(className := "page-tag", id := XmlAst.toId(tag), tag),
          Page.pageList(withTag(tag), cls = Some("post-link"))
        )
      ))
    )
