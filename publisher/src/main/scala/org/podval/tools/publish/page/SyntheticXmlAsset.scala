package org.podval.tools.publish.page

import org.podval.tools.publish.site.{Path, Site}
import org.podval.xml.{Xml, XmlDocument, XmlWriterConfig}

abstract class SyntheticXmlAsset(site: Site, path: Path) extends SyntheticAsset(site, path):
  // Element render omits the XML declaration; a document carries the canonical one.
  final override def textContent: String = XmlWriterConfig.Plain.render(XmlDocument.xml(xmlContent))

  def xmlContent: Xml.Element
