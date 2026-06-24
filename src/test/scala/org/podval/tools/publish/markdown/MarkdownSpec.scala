package org.podval.tools.publish.markdown

import org.podval.xml.{HtmlXmlDialect, Xml}
import zio.Scope
import zio.test.*

object MarkdownSpec extends ZIOSpecDefault:
  def parse(input: String, verify: Xml.Element => TestResult): TestResult =
    val parsed: Xml.Element = MarkdownMarkup.parse(input).toOption.get
    verify(parsed)

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("Markdown")(
    test("nested lists") {
      parse(
        """* TOC
          |{:toc}
          |""".stripMargin,
        xml =>

          println(HtmlXmlDialect.render(xml))
          assertTrue(
            true
          )
      )
    }
  )

