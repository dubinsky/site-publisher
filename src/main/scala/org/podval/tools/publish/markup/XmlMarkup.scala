package org.podval.tools.publish.markup

import org.podval.tools.publish.page.PageSource
import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.{Xml, XmlDialect}
import java.io.File

// Note: this exist only to parse XML to disambiguate the dialect
object XmlMarkup extends Markup(
  name = "XML",
  xmlDialect = XmlDialect.Plain,
  allowsInternalFrontMatter = false,
  rendersToXml = true,
  extension = "xml"
):
  override def xmlContent(content: String, sourceFile: File): String = content
  override def processors(ids: IdGenerator, source: PageSource): Seq[Converter] = Seq.empty
  override def retrieveTitle(xml: Xml.Element): (Xml.Element, Option[Xml.Element]) = (xml, None)
