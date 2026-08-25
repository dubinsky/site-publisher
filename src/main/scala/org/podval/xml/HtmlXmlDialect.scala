package org.podval.xml

object HtmlXmlDialect extends XmlDialect(
  preformat = Set("pre"),
  stack = Set("nav", "header", "main", "div"),
  // Phrasing wrappers: never indent children (that would become a visible HTML space).
  unStack = Set(
    "a", "abbr", "b", "bdi", "bdo", "cite", "code", "data", "dfn", "em",
    "i", "kbd", "mark", "q", "s", "samp", "small", "span", "strong", "sub",
    "sup", "time", "u", "var"
  ),
  nest = Set.empty,
  break = Set.empty, // TODO TEI: lb; HTML: br?!
  cling = Set("span"),
  // HTML void elements: no end tag, no content. XmlWriter emits <br/> for these
  // when empty; other empty elements become <script></script> (a self-closed
  // <script/> is mis-parsed). https://html.spec.whatwg.org/multipage/syntax.html#void-elements
  selfClose = Set(
    "area", "base", "br", "col", "embed", "hr", "img", "input",
    "link", "meta", "source", "track", "wbr"
  )
):
  val namespace: String = "http://www.w3.org/1999/xhtml"
