package org.podval.tools.publish.tei

import org.podval.tei.{EntityKind, TeiXmlDialect}
import org.podval.tools.publish.markup.{Converter, Markup, XmlMarkup}
import org.podval.tools.publish.page.{MarkupPage, PageSource}
import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.{Html, Xml}
import java.io.File

object TeiMarkup extends Markup(
  name = "TEI",
  xmlDialect = TeiXmlDialect,
  allowsInternalFrontMatter = false,
  rendersToXml = true,
  extension = XmlMarkup.extension
):
  override def rootElements: Set[String] = Set("TEI", "store", "collection") ++ EntityKind.values.map(_.element).toSet

  override def xmlContent(content: String, sourceFile: File): String = content

  override def processors(
    ids: IdGenerator,
    source: PageSource
  ): Seq[Converter] = Seq(
    Tei2HtmlConverter(),
    TeiEntityNamesConverter(),
    TeiFacsimileLinksConverter(),
    TeiFootnotesConverter(ids),
    TeiSectionIdsConverter(ids)
  )

  override def entityKind(xml: Xml.Element): Option[EntityKind] =
    EntityKind.values.find(entityKind => xml.getName == entityKind.element)

  override def retrieveTitle(xml: Xml.Element): (Xml.Element, Option[Xml.Element]) =
    (xml, None) // TODO
  
  override def pageHeader(page: MarkupPage): Html.Element =
    super.pageHeader(page) // TODO

