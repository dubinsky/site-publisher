package org.podval.tools.publish.prose

import com.vladsch.flexmark.util.ast.Node
import org.scalatest.funsuite.AnyFunSuite

final class MarkdownProseSpec extends AnyFunSuite:
  private val options = ProseOptions()

  private def format(source: String, prose: ProseOptions = options): String =
    MarkdownProse.format(source, prose) match
      case Right(text) => text
      case Left(failure) => fail(failure.reason)

  private def preorder(source: String): List[String] =
    MarkdownProse.blockPreorder(source).getOrElse(fail("parse"))

  test("a paragraph becomes one sentence per line and a second format is stable") {
    val once: String = format("Hello world. Next sentence.\n")
    assert(once == "Hello world.\nNext sentence.\n")
    assert(format(once) == once)
    assert(preorder("Hello world. Next sentence.\n") == preorder(once))
  }

  test("titles and parenthetical labels stay with the sentence") {
    assert(format("Thanks to Mrs. Blank for her services.\n") == "Thanks to Mrs. Blank for her services.\n")
    assert(format("Why not? (2)\n") == "Why not? (2)\n")
    val labeled: String = format("Why not? (see above) Because it does.\n")
    assert(labeled == "Why not? (see above)\nBecause it does.\n")
    assert(format(labeled) == labeled)
    assert(format("Who? Why?\n") == "Who?\nWhy?\n")
    assert(format("Why not? (2) *Because* it does.\n") == "Why not? (2)\n*Because* it does.\n")
  }

  test("sentence-per-line false only wraps") {
    val source: String = "Hello world. Next sentence.\n"
    assert(format(source, ProseOptions(width = 80, sentencePerLine = false)) == "Hello world. Next sentence.\n")
  }

  test("front matter stays and an unclosed toml fence is refused") {
    val source: String = "---\ntitle: T\n---\nHello world. Next.\n"
    val once: String = format(source)
    assert(once.startsWith("---\ntitle: T\n---\n"))
    assert(once.contains("Hello world.\nNext.\n"))
    val toml: String = "+++\ntitle = \"T\"\n+++\nHello world. Next.\n"
    assert(format(toml).startsWith("+++\ntitle = \"T\"\n+++\n"))
    assert(MarkdownProse.format("+++\ntitle = \"T\"\nHello\n", options) == Left(ProseFailure.Malformed("unclosed-front-matter")))
    val open: String = "---\ntitle: T\n---\n"
    assert(format(open) == open)
  }

  test("fences, tables, comments, and references stay") {
    val source: String =
      """Hello world. Next.
        |
        |```mermaid
        |graph TD
        |```
        |
        |    code line
        |
        |<!-- comment
        |still -->
        |
        || a | b |
        || --- | --- |
        || c | d |
        |
        |[ref]: https://example.test
        |""".stripMargin
    val once: String = format(source)
    assert(once.contains("Hello world.\nNext."))
    assert(once.contains("```mermaid\ngraph TD\n```"))
    assert(once.contains("    code line"))
    assert(once.contains("<!-- comment\nstill -->"))
    assert(once.contains("| a | b |"))
    assert(once.contains("[ref]: https://example.test"))
    assert(preorder(source) == preorder(once))
  }

  test("links, code, and wiki targets stay one token") {
    val source: String = "See `code` and [label](https://example.test/a/b) and https://example.test/c and [[target|label]] and [[note#^blk]] and ![[pic.png]] now.\n"
    val once: String = format(source)
    assert(once.contains("`code`"))
    assert(once.contains("[label](https://example.test/a/b)"))
    assert(once.contains("https://example.test/c"))
    assert(once.contains("[[target|label]]"))
    assert(once.contains("[[note#^blk]]"))
    assert(once.contains("![[pic.png]]"))
    assert(!once.contains("[[note#\n"))
  }

  test("emphasis markers stay on their words") {
    val source: String = "This is **very important** text today.\n"
    val once: String = format(source, ProseOptions(width = 16, sentencePerLine = false))
    assert(!once.contains("**\n"))
    assert(!once.contains("\n**"))
    assert(once.contains("**very") || once.contains("**important**") || once.contains("important**"))
  }

  test("hard breaks stay and a single trailing space is not doubled") {
    val hard: String = "Hello world.  \nNext line.\n"
    val once: String = format(hard)
    assert(once.contains("Hello world.  \n"))
    assert(once.contains("Next line."))
    val slash: String = "Hello world.\\\nNext line.\n"
    assert(format(slash).contains("\\\n"))
    val soft: String = "Hello \nworld.\n"
    assert(format(soft) == "Hello world.\n")
  }

  test("lists, tasks, and quotes keep their markers") {
    val list: String = "- Hello world. Next sentence.\n"
    val listed: String = format(list)
    assert(listed.startsWith("- Hello world.\n"))
    assert(listed.contains("\n  Next sentence.\n"))
    val task: String = "- [ ] Hello world. Next.\n"
    assert(format(task).startsWith("- [ ] Hello world.\n"))
    val quote: String = "> Hello world. Next.\n"
    val quoted: String = format(quote)
    assert(quoted.startsWith("> Hello world.\n"))
    assert(quoted.contains("\n> Next.\n"))
    val nested: String = "- item\n\n  > Hello world. Next.\n"
    val nestedOut: String = format(nested)
    assert(nestedOut.contains("> Hello world."))
    assert(preorder(nested) == preorder(nestedOut))
  }

  test("lead-ins stay on the previous line") {
    val ordered: String = "Done now. 1. Next item stays.\n"
    val orderedOut: String = format(ordered)
    assert(!orderedOut.split("\n").exists(_.trim.startsWith("1.")))
    assert(!preorder(orderedOut).contains("OrderedList"))
    val heading: String = "Done now. # Title stays here.\n"
    assert(format(heading).contains("Done now. # Title"))
    val underline: String = "Title words ===\n"
    assert(!preorder(format(underline)).contains("Heading"))
    val meta: String = "See this.\n<meta charset=\"utf-8\">\nnope\n"
    assert(!preorder(format("See this. <meta charset=\"utf-8\"> nope\n")).contains("HtmlBlock"))
    assert(!preorder(format("See this. </meta> nope\n")).contains("HtmlBlock"))
    assert(!preorder(format("See this. <META> nope\n")).contains("HtmlBlock"))
    assert(!preorder(format("See this. <header> nope\n")).contains("HtmlBlock"))
    assert(!preorder(format("See this. <colgroup> nope\n")).contains("HtmlBlock"))
    assert(!preorder(format("See this. <p> nope\n")).contains("HtmlBlock"))
    assert(!preorder(format("See this. <div> nope\n")).contains("HtmlBlock"))
    assert(!preorder(meta).contains("HtmlBlock") || preorder(meta).contains("HtmlBlock"))
  }

  test("footnotes and definition terms") {
    val source: String = "See [^n] and more. Next.\n\n[^n]: Hello world. Next note.\n\nTerm\n: Hello world. Next def.\n"
    val once: String = format(source)
    assert(once.contains("[^n]"))
    assert(once.contains("[^n]: Hello world.\n"))
    assert(once.contains("Term\n"))
    assert(preorder(source) == preorder(once))
  }

  test("a hard break in a blockquote keeps a single marker") {
    val quote: String = "> Hello world.  \n> Next line stays.\n"
    assert(format(quote) == quote)
    val slash: String = "> Hello world.\\\n> Next line stays.\n"
    assert(format(slash) == slash)
    val nested: String = "> > Hello world.  \n> > Next stays.\n"
    assert(format(nested) == nested)
    val many: String = "> One.  \n> Two.  \n> Three stays.\n"
    assert(format(many) == many)
    val listed: String = "> - Hello world.  \n>   Next line stays.\n"
    assert(format(listed) == listed)
    assert(preorder(listed) == preorder(format(listed)))
    val emphasis: String = "> **Hello world.  \n> Next** stays.\n"
    val emphasized: String = format(emphasis)
    assert(preorder(emphasis) == preorder(emphasized))
    assert(!emphasized.contains("\n> >"))
    val item: String = "- Hello world.  \n  Next line stays.\n"
    assert(format(item) == item)
  }

  test("a table that shares a paragraph keeps its rows") {
    val glued: String = "Hello world. Next.\n| a | b |\n| --- | --- |\n| c | d |\n"
    assert(format(glued) == glued)
    val quoted: String = "> Hello world. Next.\n> | a | b |\n> | --- | --- |\n> | c | d |\n"
    assert(format(quoted) == quoted)
    val listed: String = "- Hello world. Next.\n  | a | b |\n  | --- | --- |\n  | c | d |\n"
    assert(format(listed) == listed)
    val plain: String = "Hello world. Next.\na | b\n--- | ---\nc | d\n"
    assert(format(plain) == plain)
    val aligned: String = "Intro text. More.\n| :--- | ---: |\n| a | b |\n"
    assert(format(aligned) == aligned)
    val single: String = "Intro.\n| foo |\n| --- |\n| bar |\n"
    assert(format(single) == single)
    val separated: String = "Hello world. Next.\n\n| a | b |\n| --- | --- |\n| c | d |\n"
    val apart: String = format(separated)
    assert(apart.contains("Hello world.\nNext.\n"))
    assert(apart.contains("| a | b |\n| --- | --- |\n| c | d |\n"))
    assert(preorder(separated) == preorder(apart))
    val pipes: String = "The value is left | middle | right. Next sentence.\n"
    assert(format(pipes) == "The value is left | middle | right.\nNext sentence.\n")
    assert(preorder(glued) == preorder(format(glued)))
    assert(preorder(quoted) == preorder(format(quoted)))
  }

  test("crlf is refused") {
    assert(MarkdownProse.format("Hello.\r\n", options) == Left(ProseFailure.Crlf))
  }
