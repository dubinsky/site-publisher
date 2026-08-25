package org.podval.tools.publish.page

import org.scalatest.funsuite.AnyFunSuite
import java.time.LocalDate

final class FrontMatterSpec extends AnyFunSuite:
  private given CanEqual[LocalDate, LocalDate] = CanEqual.derived

  private def roundTrip(input: String): Unit =
    val parsed: FrontMatter = FrontMatter.parse(FrontMatter.split(input)._1).toOption.get
    val rendered: String = parsed.write
    val reparsed: FrontMatter = FrontMatter.parse(FrontMatter.split(rendered)._1).toOption.get
    val rerendered: String = reparsed.write
    assert(rendered == rerendered)

  private def parse(input: String): (FrontMatter, String) =
    val (frontMatterInput, content) = FrontMatter.split(input)
    (FrontMatter.parse(frontMatterInput).toOption.get, content)

  test("empty FrontMatter") {
    val (_, content) = parse(
      """---
        |---
        |# Hello
        |""".stripMargin
    )
    assert(content ==
      """
        |
        |# Hello
        |""".stripMargin
    )
  }

  test("non-empty FrontMatter") {
    val (_, content) = parse(
      """---
        |title: Hello
        |date: 2026-03-22
        |tags: [yaml, markdown, test]
        |---
        |# Hello
        |""".stripMargin
    )
    assert(content ==
      """
        |
        |
        |
        |
        |# Hello
        |""".stripMargin
    )
  }

  test("FrontMatter must be a mapping") {
    val error: Throwable = FrontMatter.parse(FrontMatter.split(
      """---
        |[yaml, markdown, test]
        |---
        |# Hello
        |""".stripMargin
    )._1).left.toOption.get
    assert(error.getMessage.contains("Expected mapping for record"))
  }

  test("round-trip without FrontMatter") {
    roundTrip("# Hello\n")
  }

  test("round-trip with FrontMatter") {
    roundTrip(
      """---
        |title: Hello
        |date: 2026-03-22
        |tags: [yaml, markdown, test]
        |xxx: true
        |---
        |# Hello
        |""".stripMargin
    )
  }

  test("FrontMatter keys") {
    val (frontMatter, _) = parse(
      """---
        |title: Hello
        |date: '2026-03-22T14:17:00.001-04:00'
        |tags: [yaml, markdown, test]
        |categories: [important]
        |xxx: true
        |---
        |# Hello
        |""".stripMargin
    )
    assert(frontMatter.title.contains("Hello"))
    assert(frontMatter.tags == List("yaml", "markdown", "test"))
    assert(frontMatter.categories == List("important"))
    assert(frontMatter.date.map(_.localDate).contains(LocalDate.of(2026, 3, 22)))
    assert(frontMatter.bibliography.isEmpty)
    assert(frontMatter.csl.isEmpty)
  }
