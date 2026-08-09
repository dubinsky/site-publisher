package org.podval.tools.publish.markup

import org.podval.xml.{HtmlXmlDialect, Xml, XmlParser}
import zio.Scope
import zio.test.*

object MarkdownSpec extends ZIOSpecDefault:
  def parse(input: String, verify: Xml.Element => TestResult): TestResult =
    val xmlString: String = MarkdownMarkup.xmlContent(
      input,
      null,
      null
    )
    val parsed: Xml.Element = XmlParser.parseXml(xmlString).toOption.get
    verify(parsed)

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("Markdown")(
    test("nested lists") {
      parse(
        """* TOC
          |{:toc}
          |""".stripMargin,
        xml =>
          assertTrue(
            true
          )
      )
    }
  )

