package org.podval.tools.publish.markup

import org.podval.xml.XmlWriterConfig

object TeiXmlWriterConfig extends XmlWriterConfig(
  unStack = Set("choice"),
  nest = Set("p", /*"abstract",*/ "head", "salute", "dateline"),
  cling = Set("note", "lb", "sic", "corr")
)
