package org.podval.tools.publish

import org.podval.xml.{HtmlXmlDialect, Xml}
import zio.Scope
import zio.test.*

object MarkdownSpec extends ZIOSpecDefault:
  def parse(input: String, verify: Xml.Element => TestResult): TestResult =
    val parsed = MarkdownMarkup.parse(input, TestErrorReporter())
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

