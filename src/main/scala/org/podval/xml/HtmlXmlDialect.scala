package org.podval.xml

object HtmlXmlDialect extends XmlDialect(
  stop = Set("code"),
  preformat = Set("pre"),
  stack = Set("nav", "header", "main", "div"),
  unStack = Set.empty,
  nest = Set.empty,
  break = Set.empty, // TODO TEI: lb; HTML: br?!
  cling = Set.empty, // TODO Set("span")?
  selfClose = Set("br", "hr", "meta", "link", "img", "input")
)
