package org.podval.tools.publish.page

import org.podval.tei.EntityKind
import org.podval.tools.publish.markup.{Blocks, Ids, Links, Markup, Toc}
import org.podval.tools.publish.page.PageSource
import org.podval.xml.{Xml, XmlDialect}

final class PageContent(
  source: PageSource,
  val frontMatter: FrontMatter,
  val title: Option[Xml.Element],
  val xml: Xml.Element
):
  def page: OriginalMarkupPage = source.page
  def markup: Markup = source.markup
  def xmlDialect: XmlDialect = source.xmlDialect
  def entityKind: Option[EntityKind] = markup.entityKind(xml)

  lazy val toc: Toc = Toc(xml, source)
  lazy val ids: Ids = Ids(xml, xmlDialect)
  lazy val blocks: Blocks = Blocks(xml, xmlDialect, source)

  lazy val xmlResolved: Xml.Element =
    var result: Xml.Element = markup.postProcess(source, xml)
    result = Links.resolveInternalLinks(result, xmlDialect, page, source)
    result
