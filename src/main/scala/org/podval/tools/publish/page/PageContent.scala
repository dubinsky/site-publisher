package org.podval.tools.publish.page

import org.podval.tei.EntityKind
import org.podval.tools.publish.PageError
import org.podval.tools.publish.link.{BackLink, Fragment, Link, Toc}
import org.podval.tools.publish.markup.Links
import org.podval.xml.{Html, Xml}

final class PageContent(
  val source: PageSource,
  val frontMatter: FrontMatter,
  val title: Option[Xml.Element],
  xml: Xml.Element
):
  def entityKind: Option[EntityKind] = source.markupKind.entityKind(xml)

  def toHtml(doAddToc: Boolean): Html.Element = source.markup.toHtml(source, xml, toc, doAddToc)

  lazy val toc: Toc = Toc(source.markupKind.sections(source, xml))

  def resolveId(id: String): Option[Link.ToId] = ids.find(_ == id).map(Link.ToId(_))

  private lazy val ids: Seq[String] = source.xmlDialect.gather(xml, _.getId)

  def resolveBlock(id: String): Option[Link.ToBlock] = blocks.find(_.id == id).map(Link.ToBlock(_))

  private lazy val blocks: Seq[Fragment.Block] = source.xmlDialect.gather(xml, element =>
    if !Links.isBlock(element) then None else element
      .getId
      .map(Fragment.Block(_))
      .orElse:
        source.error(PageError.NoId, s"Defect: No id on block $element")
        None
  )

  def backLinks: Seq[BackLink] = source.xmlDialect.gatherWithParents(
    element = xml,
    gatherElement = (element, parents) => BackLink(
      element = element,
      parents = parents,
      from = source.page,
      content = this
    )
  )


