package org.podval.tools.publish.page

import org.podval.tei.EntityKind
import org.podval.tools.publish.PageError
import org.podval.tools.publish.link.{BackLink, Fragment, Toc}
import org.podval.tools.publish.markup.Links
import org.podval.xml.{Html, Xml}

final class PageContent(
  val source: PageSource,
  val frontMatter: FrontMatter,
  xml: Xml.Element
):
  def entityKind: Option[EntityKind] = source.markupKind.entityKind(xml)

  def backLinks: Seq[BackLink] =
    source.xmlDialect.gatherWithParents(
      element = xml,
      gatherElement = BackLink(_, _, source.page, toc)
    )

  def toHtml: Html.Element = source.markup.processors.toHtml(source, xml, toc)

  lazy val toc: Toc = Toc(
    sections = source.markupKind.sections(source, xml),
    ids = source.xmlDialect.gather(xml, _.getId),
    blocks = source.xmlDialect.gather(xml, element =>
      if !Links.isBlock(element)
      then None
      else element
        .getId
        .map(Fragment.Block(_))
        .orElse:
          source.error(PageError.NoId, s"Defect: No id on block $element")
          None
    )
  )
