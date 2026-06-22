package org.podval.tools.publish.markup

import org.podval.tei.{EntityKind, TeiXmlDialect}
import org.podval.tools.publish.feature.*
import org.podval.tools.publish.link.Fragment
import org.podval.tools.publish.page.PageContent
import org.podval.tools.publish.processor.Processors
import org.podval.xml.{Html, Xml, XmlDialect}

object TeiMarkup extends XmlLikeMarkup:
  override val additionalExtensions: Set[String] = Set.empty
  override def xmlDialect: XmlDialect = TeiXmlDialect

  override def entityKind(xml: Xml.Element): Option[EntityKind] =
    EntityKind.values.find(entityKind => xml.getName == entityKind.element)

  def processors: Processors = Processors(
    new Tei2HtmlConverter,
    new TeiEntityNamesConverter,
    new TeiFacsimileLinksConverter,
    new TeiFootnotesConverter,
    new TeiSectionIdsConverter,
    new AnchorIdsConverter,
    new InternalLinksProcessor,
    new FootnotesTransformer
  )

  override def sections(
    content: PageContent
  ): Seq[Fragment.Section] = Seq.empty // TODO

  // TODO !!!
  override def pageHeader(content: PageContent): Html.Element = Markup.pageHeader(content)
