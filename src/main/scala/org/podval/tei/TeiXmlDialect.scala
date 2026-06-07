package org.podval.tei

import org.podval.xml.XmlDialect

object TeiXmlDialect extends XmlDialect(
  name = "TEI",
  root = Set("TEI", "store", "collection", "place", "person", "org"),
  stop = Set.empty,
  preformat = Set.empty,
  stack = Set.empty,
  unStack = Set("choice"),
  nest = Set("p", /*"abstract",*/ "head", "salute", "dateline"),
  break = Set.empty,
  cling = Set("note", "lb", "sic", "corr"),
  selfClose = Set.empty,
  encodeXmlSpecials = false
)
