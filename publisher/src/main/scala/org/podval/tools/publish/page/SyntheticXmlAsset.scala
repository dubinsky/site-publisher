package org.podval.tools.publish.page

import org.podval.tools.publish.site.{Path, Site}
import org.podval.xml.{Xml, XmlWriterConfig}

abstract class SyntheticXmlAsset(site: Site, path: Path) extends SyntheticAsset(site, path):
  final override def textContent: String = XmlWriterConfig.Plain.render(xmlContent)

  def xmlContent: Xml.Element
