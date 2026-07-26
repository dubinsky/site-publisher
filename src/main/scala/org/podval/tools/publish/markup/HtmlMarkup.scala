package org.podval.tools.publish.markup

import org.podval.tools.publish.link.Fragment.Section
import org.podval.tools.publish.page.PageContent
import org.podval.xml.{Html, HtmlXmlDialect}

object HtmlMarkup extends MarkupKind(
  name = "HTML",
  allowsInternalFrontMatter = true,
  extension = "html",
  rendersToXml = false,
  xmlDialect = HtmlXmlDialect,
):
  override def pageHeader(content: PageContent): Html.Element = MarkupKind.pageHeader(content)

  override def sections(content: PageContent): Seq[Section] = HtmlSections.sections(content)
