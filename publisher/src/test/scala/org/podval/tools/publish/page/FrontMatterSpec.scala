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
    assert(!frontMatter.asset)
  }

  test("asset flag") {
    val (frontMatter, _) = parse(
      """---
        |asset: true
        |---
        |body
        |""".stripMargin
    )
    assert(frontMatter.asset)
  }

  test("write omits default false and empty lists and keeps an extra key") {
    val parsed: FrontMatter = FrontMatter.parse(Some(
      """title: Hello
        |math: false
        |tags: []
        |xxx: true
        |""".stripMargin
    )).toOption.get
    val written: String = parsed.write
    assert(written.contains("title: Hello"), written)
    assert(written.contains("xxx:"), written)
    assert(!written.contains("math:"), written)
    assert(!written.contains("tags:"), written)
    assert(!written.contains("asset:"), written)
    val again: FrontMatter = FrontMatter.parse(Some(written.stripPrefix("---\n").stripSuffix("---\n"))).toOption.get
    assert(again.equals(parsed))
  }

  test("write keeps math true") {
    val written: String = FrontMatter.parse(Some("math: true\n")).toOption.get.write
    assert(written.contains("math: true"), written)
  }

  test("a non-date modified_time stays stashed and does not reject the page") {
    val parsed: FrontMatter = FrontMatter.parse(Some(
      """title: Hello
        |modified_time:
        |  nested: true
        |xxx: 1
        |""".stripMargin
    )).toOption.get
    assert(parsed.title.contains("Hello"))
    assert(parsed.modifiedTime.isEmpty)
    assert(parsed.copy().equals(parsed))
    val written: String = parsed.write
    assert(written.contains("modified_time:"), written)
    assert(written.contains("xxx:"), written)
    assert(!written.contains("math:"), written)
  }

  test("modified_time reads a date and copy keeps it") {
    val parsed: FrontMatter = FrontMatter.parse(Some(
      "title: Hello\nmodified_time: 2010-01-28T14:24:00.004-05:00\n"
    )).toOption.get
    assert(parsed.modifiedTime.map(_.toString).contains("2010-01-28T14:24:00.004-05:00"))
    assert(parsed.write.contains("modified_time:"))
    assert(parsed.copy().equals(parsed))
  }

  test("two parses of the same extras are equal") {
    val text: String = "title: Hello\nxxx: true\n"
    val a: FrontMatter = FrontMatter.parse(Some(text)).toOption.get
    val b: FrontMatter = FrontMatter.parse(Some(text)).toOption.get
    assert(a.equals(b))
    assert(a.copy().equals(a))
  }
