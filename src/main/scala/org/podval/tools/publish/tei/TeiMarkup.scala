package org.podval.tools.publish.tei

import org.podval.tei.{EntityKind, TeiXmlDialect}
import org.podval.tools.publish.link.Fragment
import org.podval.tools.publish.markup.{MarkupKind, XmlLikeMarkup}
import org.podval.tools.publish.page.PageContent
import org.podval.xml.{Html, Xml}

object TeiMarkup extends XmlLikeMarkup(
  name = "TEI",
  xmlDialect = TeiXmlDialect
):
  override def entityKind(xml: Xml.Element): Option[EntityKind] =
    EntityKind.values.find(entityKind => xml.getName == entityKind.element)

  override def sections(
    content: PageContent
  ): Seq[Fragment.Section] = Seq.empty // TODO

  // TODO !!!
  override def pageHeader(content: PageContent): Html.Element =
    MarkupKind.pageHeader(content)
