package org.podval.tools.publish.markup

import org.podval.tools.publish.site.PageErrorReporter
import org.podval.xml.{Xml, XmlDialect}
import java.io.File

// Note: this exists only to parse XML to disambiguate the dialect
object XmlMarkup extends Markup(
  name = "XML",
  xmlDialect = XmlDialect.Plain,
  rendersToXml = true,
  extension = "xml"
):
  override def xmlContent(content: String, sourceFile: File): String = content

  override def isSectionHeader(element: Xml.Element): Boolean = false

  override def process(
    xml: Xml.Element,
    errorReporter: PageErrorReporter
  ): (Xml.Element, Option[Xml.Element]) =
    (xml, None)