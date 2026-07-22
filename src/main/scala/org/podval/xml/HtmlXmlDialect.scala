package org.podval.xml

object HtmlXmlDialect extends XmlDialect(
  stop = Set("code"),
  preformat = Set("pre"),
  stack = Set("nav", "header", "main", "div"),
  unStack = Set.empty,
  nest = Set.empty,
  break = Set.empty, // TODO TEI: lb; HTML: br?!
  cling = Set.empty, // TODO Set("span")?
  // TODO I think the full list of HTML5 void elements is:
  //val voidTags = Set(
  //  "area", "base", "br", "col", "embed", "hr", "img", "input",
  //  "link", "meta", "param", "source", "track", "wbr"
  //)
  // https://html.spec.whatwg.org/multipage/syntax.html#void-elements
  selfClose = Set("br", "hr", "meta", "link", "img", "input")
):
  val namespace: String = "http://www.w3.org/1999/xhtml"
