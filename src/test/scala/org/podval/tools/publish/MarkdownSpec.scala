package org.podval.tools.publish

import org.podval.xml.Xml
import zio.Scope
import zio.test.*

object MarkdownSpec extends ZIOSpecDefault:
  def parse(input: String, verify: Xml.Element => TestResult): TestResult =
    val parsed = Markdown.parse(input, TestErrorReporter())
    verify(parsed)

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("Markdown")(
    test("nested lists") {
      parse(
        """* TOC
          |{:toc}
          |""".stripMargin,
        xml =>

          println(Xml.writer.render(xml))
          assertTrue(
            true
          )
      )
    }
  )

