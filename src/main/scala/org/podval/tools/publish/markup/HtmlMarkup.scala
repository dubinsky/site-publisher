package org.podval.tools.publish.markup

import org.podval.tools.publish.link.Fragment.Section
import org.podval.tools.publish.page.{MarkupPage, PageSource}
import org.podval.xml.{Html, HtmlXmlDialect, Xml}

// TODO merge with HtmlSections; split converter out; move into the 'html' package.
object HtmlMarkup extends MarkupKind(
  name = "HTML",
  allowsInternalFrontMatter = true,
  extension = "html",
  rendersToXml = false,
  xmlDialect = HtmlXmlDialect,
):
  override def retrieveTitle(xml: Xml.Element): (Xml.Element, Option[Xml.Element]) = HtmlSections.retrieveTitle(xml)

  override def pageHeader(page: MarkupPage): Html.Element = MarkupKind.pageHeader(page)

  override def sections(source: PageSource, xml: Xml.Element): Seq[Section] = HtmlSections.sections(source, xml)
