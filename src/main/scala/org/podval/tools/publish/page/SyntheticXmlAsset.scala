package org.podval.tools.publish.page

import org.podval.tools.publish.site.{Path, Site}
import org.podval.xml.{Xml, XmlDialect}

abstract class SyntheticXmlAsset(site: Site, path: Path) extends SyntheticAsset(site, path):
  final override def textContent: String = XmlDialect.Plain.render(xmlContent)

  def xmlContent: Xml.Element
