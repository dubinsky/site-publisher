package org.podval.tools.publish.page

import org.podval.tei.EntityKind
import org.podval.tools.publish.markup.{BackLink, Blocks, Ids, Link, Links, Toc}
import org.podval.tools.publish.page.PageSource
import org.podval.xml.{Html, Xml, Xml2Html}

final class PageContent(
  val source: PageSource,
  val frontMatter: FrontMatter,
  val title: Option[Xml.Element],
  xml: Xml.Element
):
  def entityKind: Option[EntityKind] = source.markup.entityKind(xml)

  lazy val toc: Toc = Toc(xml, source)

  private lazy val ids: Ids = Ids(xml, source.xmlDialect)
  def resolveId(id: String): Option[Link.ToId] = ids.resolve(id)

  private lazy val blocks: Blocks = Blocks(xml, source.xmlDialect, source)
  def resolveBlock(id: String): Option[Link.ToBlock] = blocks.resolve(id)

  def backLinks: Seq[BackLink] = BackLink.backLinks(
    xml,
    source.xmlDialect,
    source.page,
    ids
  )
  
  // TODO maybe make it a lazy val?
  private def xmlFinal: Xml.Element =
    var result: Xml.Element = source.markup.postProcess(source, xml)
    // TODO move into Links
    result = source.xmlDialect.transform(result, element => Links.resolveInternalLinks(element, source.page, source).getOrElse(element))
    result

  def toHtml(
    sectionId: Option[String],
    isTerminal: Boolean
  ): Html.Element =
    // Select XML to include
    val xmlIncluded: Xml.Element = toc.select(
      xml = xmlFinal,
      sectionId = sectionId,
      isTerminal = isTerminal,
      xmlDialect = source.xmlDialect
    )

    // Convert to HTML
    val html: Html.Element = Xml2Html.fromXml(xmlIncluded)

    // Add TOC to HTML
    toc.add(
      html,
      hasToc = source.page.hasToc,
      tocDepth = source.page.tocDepth,
      sectionId,
      source.markup
    )
