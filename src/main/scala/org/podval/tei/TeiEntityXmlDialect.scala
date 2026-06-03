package org.podval.tei

import org.podval.xml.XmlDialect

object TeiEntityXmlDialect extends XmlDialect(
  name = "TEI Entity",
  root = Set("place", "person", "org")
)
