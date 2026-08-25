package org.podval.tei

import org.podval.xml.XmlDialect

object TeiXmlDialect extends XmlDialect(
  unStack = Set("choice"),
  nest = Set("p", /*"abstract",*/ "head", "salute", "dateline"),
  cling = Set("note", "lb", "sic", "corr")
)
