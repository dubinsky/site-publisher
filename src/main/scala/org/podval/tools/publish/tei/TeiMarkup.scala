package org.podval.tools.publish.tei

import org.podval.tei.{EntityKind, TeiXmlDialect}
import org.podval.tools.publish.link.Fragment
import org.podval.tools.publish.markup.{MarkupKind, XmlMarkup}
import org.podval.tools.publish.page.{MarkupPage, PageSource}
import org.podval.tools.publish.processor.Processor
import org.podval.xml.{Html, Xml}

object TeiMarkup extends MarkupKind(
  name = "TEI",
  xmlDialect = TeiXmlDialect,
  allowsInternalFrontMatter = false,
  rendersToXml = true,
  extension = XmlMarkup.extension
):
  override def entityKind(xml: Xml.Element): Option[EntityKind] =
    EntityKind.values.find(entityKind => xml.getName == entityKind.element)

  override def sections(
    source: PageSource, xml: Xml.Element
  ): Seq[Fragment.Section] = Seq.empty // TODO

  // TODO !!!
  override def pageHeader(page: MarkupPage): Html.Element =
    MarkupKind.pageHeader(page)

  def processors: Seq[Processor] = Seq(
    new Tei2HtmlConverter,
    new TeiEntityNamesConverter,
    new TeiFacsimileLinksConverter,
    new TeiFootnotesConverter,
    new TeiSectionIdsConverter
  )
  