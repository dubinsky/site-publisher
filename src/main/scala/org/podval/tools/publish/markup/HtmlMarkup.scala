package org.podval.tools.publish.markup

import org.podval.tools.publish.link.Fragment.Section
import org.podval.tools.publish.page.{MarkupPage, PageSource}
import org.podval.xml.{Html, HtmlXmlDialect, Xml}

object HtmlMarkup extends MarkupKind(
  name = "HTML",
  allowsInternalFrontMatter = true,
  extension = "html",
  rendersToXml = false,
  xmlDialect = HtmlXmlDialect,
):
  override def pageHeader(page: MarkupPage): Html.Element = MarkupKind.pageHeader(page)

  override def sections(source: PageSource, xml: Xml.Element): Seq[Section] = HtmlSections.sections(source, xml)
