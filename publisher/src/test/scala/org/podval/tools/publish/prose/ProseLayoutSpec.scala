package org.podval.tools.publish.prose

import org.scalatest.funsuite.AnyFunSuite
import ProseLayout.Token
import ProseLayout.Token.{Spaces, Word}

final class ProseLayoutSpec extends AnyFunSuite:
  private val sentences = ProseOptions(width = 120, sentencePerLine = true)
  private val wrapped = ProseOptions(width = 120, sentencePerLine = false)
  private def noLead(text: String): Boolean = false

  private def words(text: String): List[Token] =
    text.split(" ").toList.flatMap: word =>
      if word.isEmpty then Nil else List(Word(word), Spaces(" "))
    .dropRight(1)

  test("sentences start on their own line") {
    val once: String = ProseLayout.layout(words("One two. Three four."), sentences, 0, "", noLead)
    assert(once == "One two.\nThree four.")
    val again: String = ProseLayout.layout(words(once.replace("\n", " ")), sentences, 0, "", noLead)
    assert(again == once)
  }

  test("sentence-per-line can be turned off") {
    val text: String = "One two. Three four."
    assert(ProseLayout.layout(words(text), wrapped, 0, "", noLead) == text)
  }

  test("abbreviations and versions are not sentence ends") {
    val text: String = "See e.g. this. See i.e. that. See etc. more. See vs. less. See Dr. Who. See e.g.) this. See Mr. Smith. Version 0.2.0 stays. Value 3.14 stays. Price 3.14. Next."
    val laid: String = ProseLayout.layout(words(text), sentences, 0, "", noLead)
    assert(laid.contains("See e.g. this."))
    assert(laid.contains("See Dr. Who."))
    assert(laid.contains("See e.g.) this."))
    assert(laid.contains("See Mr.\nSmith."))
    assert(laid.contains("Version 0.2.0 stays."))
    assert(laid.contains("Value 3.14 stays."))
    assert(laid.contains("Price 3.14.\nNext."))
  }

  test("closers stay on the sentence") {
    val laid: String = ProseLayout.layout(words("""Say "hi." Next. Say end.) Next. Say end?” Next."""), sentences, 0, "", noLead)
    assert(laid.startsWith("Say \"hi.\"\nNext."))
    assert(laid.contains("Say end.)\nNext."))
    assert(laid.contains("Say end?”\nNext."))
  }

  test("width wraps at the last space and width 0 does not") {
    val text: String = "alpha beta gamma"
    assert(ProseLayout.layout(words(text), ProseOptions(10, false), 0, "", noLead) == "alpha beta\ngamma")
    assert(ProseLayout.layout(words(text), ProseOptions(0, false), 0, "", noLead) == text)
  }

  test("an atom wider than the width stays whole") {
    val tokens: List[Token] = List(Word("see"), Spaces(" "), Token.Atom("https://example.test/very/long"))
    val laid: String = ProseLayout.layout(tokens, ProseOptions(8, false), 0, "", noLead)
    assert(laid.contains("https://example.test/very/long"))
    assert(!laid.contains("https://example\n"))
  }

  test("a lead-in stays on the previous line") {
    val ordered: String = ProseLayout.layout(words("Done. 1. Next item"), sentences, 0, "", MarkdownLeadIn.matches)
    assert(!ordered.split("\n").exists(_.startsWith("1.")))
    val heading: String = ProseLayout.layout(words("Done. # Title here"), sentences, 0, "", MarkdownLeadIn.matches)
    assert(heading == "Done. # Title here")
    assert(!heading.contains("\\"))
  }
