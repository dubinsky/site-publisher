package org.podval.tools.publish.tei

import org.podval.tei.{EntityKind, TeiXmlDialect}
import org.podval.tools.publish.link.Fragment
import org.podval.tools.publish.markup.{Markup, XmlLikeMarkup}
import org.podval.tools.publish.page.PageContent
import org.podval.tools.publish.processor.SingleProcessor
import org.podval.xml.{Html, Xml, XmlDialect}

final class TeiMarkup(processors: Seq[SingleProcessor]) extends XmlLikeMarkup(
  TeiXmlDialect,
  processors
):
  override def entityKind(xml: Xml.Element): Option[EntityKind] =
    EntityKind.values.find(entityKind => xml.getName == entityKind.element)

  override def sections(
    content: PageContent
  ): Seq[Fragment.Section] = Seq.empty // TODO

  // TODO !!!
  override def pageHeader(content: PageContent): Html.Element = Markup.pageHeader(content)
