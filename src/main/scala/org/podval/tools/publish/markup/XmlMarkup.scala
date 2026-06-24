package org.podval.tools.publish.markup

import org.podval.tools.publish.link.Fragment
import org.podval.tools.publish.page.PageContent
import org.podval.xml.{Html, XmlDialect}

// Note: this exist only to parse XML to disambiguate the dialect
object XmlMarkup extends XmlLikeMarkup(
  name = "XML",
  xmlDialect = XmlDialect.Plain
):
  override def pageHeader(content: PageContent): Html.Element = MarkupKind.pageHeader(content)
  override def sections(content: PageContent): Seq[Fragment.Section] = Seq.empty
