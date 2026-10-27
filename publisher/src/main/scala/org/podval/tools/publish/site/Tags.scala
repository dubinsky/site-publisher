package org.podval.tools.publish.site

import org.podval.metadata.Language
import org.podval.tools.publish.page.{FullMarkupPage, Page, SyntheticMarkupPage}
import org.podval.tools.publish.util.{Icon, IdGenerator}
import org.podval.xml.Xml
import org.podval.xml.dsl.{*, given}

final class Tags(site: Site) extends SyntheticMarkupPage(site, Path("tags").html):
  override def listedInDirectory: Boolean = true

  override def titleDefault: String =
    given Language.Spec = site.languageSpec
    Messages.text("pages.tags")

  override protected def descriptionDefault: Option[String] =
    given Language.Spec = site.languageSpec
    Some(Messages.text("pages.byTag"))
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
    href := s"$path#${IdGenerator.toId(tag)}",
    Icon.tag.html,
    tag
  )

  override protected def syntheticContent: Xml.Element =
    given Language.Spec = site.languageSpec
    div(className := "tags",
      h2(Messages.text("pages.tagsAll")),
      p(tagsAll.map(tagRef)),
      h2(Messages.text("pages.byTag")),
      ul(tagsAll.map(tag =>
        li(
          h3(className := "page-tag", id := IdGenerator.toId(tag), tag),
          Page.pageList(withTag(tag), cls = Some("post-link"))
        )
      ))
    )
