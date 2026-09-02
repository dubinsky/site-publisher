package org.podval.xml

import org.scalatest.funsuite.AnyFunSuite
import zio.blocks.chunk.Chunk

final class XmlUtilSpec extends AnyFunSuite:
  private def parse(input: String): Xml.Element =
    XmlParser.parseXml(input).toOption.get

  test("convertElements keeps mixed text and elements") {
    val converted: Xml.Nodes = XmlUtil.convertElements(parse("<p>a<x/>b</p>").getChildren, _ => None)
    assert(converted.flatMap(_.asText) == Seq("a", "b"))
    assert(converted.flatMap(_.asElement).map(_.getName) == Seq("x"))
  }

  test("convertElements can expand an element among text") {
    val converted: Xml.Nodes = XmlUtil.convertElements(
      parse("<p>a<note/>b</p>").getChildren,
      el => Option.when(el.getName == "note")(Chunk(Xml.element("span"), Xml.element("aside")))
    )
    assert(converted.flatMap(_.asText) == Seq("a", "b"))
    assert(converted.flatMap(_.asElement).map(_.getName) == Seq("span", "aside"))
  }

  test("xml2html keeps mixed text and elements") {
    val dumped: String = HtmlXmlDialect.render(XmlUtil.xml2html(parse("<p>a<x/>b</p>")))
    assert(dumped.contains("a"), dumped)
    assert(dumped.contains("b"), dumped)
    assert(dumped.contains("<x"), dumped)
  }

  test("xml2html keeps attributes and nesting") {
    val html: Html.Element = XmlUtil.xml2html(parse("""<div xml:id="x" class="y"><p>a</p></div>"""))
    assert(html.getName == "div")
    assert(html.get("xml:id").contains("x"))
    assert(html.get("class").contains("y"))
    assert(html.getChildren.flatMap(_.asElement).map(_.getName) == Seq("p"))
  }

  test("xml2html keeps empty elements") {
    val html: Html.Element = XmlUtil.xml2html(parse("<x/>"))
    assert(html.getName == "x")
    assert(html.getChildren.isEmpty)
  }

  test("xml2html drops comments and processing instructions") {
    val html: Html.Element = XmlUtil.xml2html(parse("<p>a<!--c--><?pi d?>b</p>"))
    assert(html.getChildren.flatMap(_.asAtom) == Seq("a", "b"))
    assert(html.getChildren.flatMap(_.asElement).isEmpty)
  }

  test("xml2html turns CDATA into escaped HTML text") {
    val xml: Xml.Element = Xml.element("p").setChildren(Seq(Xml.cdata("a<b")))
    val html: Html.Element = XmlUtil.xml2html(xml)
    assert(html.getChildren.flatMap(_.asAtom) == Seq("a&lt;b"))
    val dumped: String = HtmlXmlDialect.render(html)
    assert(dumped.contains("a&lt;b"), dumped)
    assert(!dumped.contains("a<b"), dumped)
  }
