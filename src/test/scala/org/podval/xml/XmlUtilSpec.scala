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
