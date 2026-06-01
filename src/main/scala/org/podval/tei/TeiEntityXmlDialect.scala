package org.podval.tei

import org.podval.xml.XmlDialect

object TeiEntityXmlDialect extends XmlDialect(
  root = Set("place", "person", "org")
)
