package org.podval.tools.publish.markup

import org.podval.tools.publish.link.{Fragment, Toc}
import org.podval.tools.publish.page.PageSource
import org.podval.tools.publish.site.{Path, Site}
import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.{Html, Xml, XmlDialect}

// Note: this exist only to parse XML to disambiguate the dialect
object XmlMarkup extends Markup(
  name = "XML",
  xmlDialect = XmlDialect.Plain,
  allowsInternalFrontMatter = false,
  rendersToXml = true,
  extension = "xml"
):
  override def xmlContent(site: Site, sourcePath: Path, content: String): String = content
  override def converters(ids: IdGenerator, source: PageSource): Seq[Converter] = Seq.empty
  override def retrieveTitle(xml: Xml.Element): (Xml.Element, Option[Xml.Element]) = (xml, None)
  override def sections(source: PageSource, xml: Xml.Element): Seq[Fragment.Section] = Seq.empty
  override def section(xml: Xml.Element, sectionId: String, toc: Toc): Xml.Element = xml
