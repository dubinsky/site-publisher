package org.podval.tools.publish.markup

import org.podval.xml.{Xml, XmlParser}
import org.scalatest.funsuite.AnyFunSuite

final class MarkdownSpec extends AnyFunSuite:
  private def parse(input: String): Xml.Element =
    val xmlString: String = MarkdownMarkup.xmlContent(input, java.io.File("t.md"))
    XmlParser.parseXml(xmlString).toOption.get

  test("nested lists") {
    val xml: Xml.Element = parse(
      """* TOC
        |{:toc}
        |""".stripMargin
    )
    assert(xml.getName == "div")
  }
