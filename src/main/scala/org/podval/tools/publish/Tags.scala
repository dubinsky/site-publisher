package org.podval.tools.publish

import org.podval.tools.publish.util.Icon
import org.podval.xml.{Html, Xml}
import zio.blocks.html.*

final class Tags(site: Site) extends MarkupPage.WithSyntheticContent(site, Path("tags").html) with Page.NonDirectory:
  override def titleDefault: String = "Tags"
  override protected def descriptionDefault: Option[String] = Some("Pages by tags")
  override protected def iconDefault: Icon = Icon.tags
  override protected def headerPagePriorityDefault: Int = 2
  override protected def langDefault: Option[String] = Some("en")

  private def tagsAll: List[String] = site.markupPages.flatMap(_.tags).distinct.sorted

  private def withTag(tag: String): List[Page] = site.markupPages.filter(_.tags.contains(tag)).sortBy(_.title)

  def tagRef(tag: String): Html.Element = a(
    className := "page-tag",
    href := s"$path#${Xml.Id.toId(tag)}",
    Icon.tag.html,
    tag
  )

  override protected def syntheticContent: Html.Element =
    div(className := "tags",
      h2("All tags"),
      p(tagsAll.map(tagRef)),
      h2("Pages by tags"),
      ul(tagsAll.map(tag =>
        li(
          h3(className := "page-tag", id := Xml.Id.toId(tag), tag),
          Page.pageList(withTag(tag), cls = Some("post-link"))
        )
      ))
    )
