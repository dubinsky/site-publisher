package org.podval.tools.publish.markup

import org.podval.tools.publish.link.Fragment
import org.podval.tools.publish.page.{MarkupPage, PageSource}
import org.podval.xml.{Html, Xml, XmlDialect}

// Note: this exist only to parse XML to disambiguate the dialect
object XmlMarkup extends MarkupKind(
  name = "XML",
  xmlDialect = XmlDialect.Plain,
  allowsInternalFrontMatter = false,
  rendersToXml = true,
  extension = "xml"
):
  override def pageHeader(page: MarkupPage): Html.Element = MarkupKind.pageHeader(page)
  override def sections(source: PageSource, xml: Xml.Element): Seq[Fragment.Section] = Seq.empty
