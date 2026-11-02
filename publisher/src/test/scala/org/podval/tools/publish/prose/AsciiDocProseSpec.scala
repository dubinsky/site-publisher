package org.podval.tools.publish.prose

import org.scalatest.funsuite.AnyFunSuite

final class AsciiDocProseSpec extends AnyFunSuite:
  private val options = ProseOptions()

  private def format(source: String): String =
    AsciiDocProse.format(source, options) match
      case Right(text) => text
      case Left(failure) => fail(failure.reason)

  private def structure(source: String): List[String] =
    AsciiDocProse.structure(source).getOrElse(fail("parse")).map: line =>
      val cut: Int = line.indexOf(':')
      line.take(cut + 1) + line.drop(cut + 1).replace("\n", " ")

  test("includes and conditionals stay, and a separated body reflows") {
    val include: String = "Hello world. Next.\n\ninclude::chap.adoc[]\n\ninclude::missing.adoc[]\n"
    val included: String = format(include)
    assert(included.contains("include::chap.adoc[]"))
    assert(included.contains("include::missing.adoc[]"))
    assert(!included.contains("Unresolved directive"))
    assert(included.contains("Hello world.\nNext."))
    val packed: String = "ifdef::x[]\nHello world. Next.\nendif::[]\n"
    assert(format(packed) == packed)
    val separated: String = "ifdef::x[]\n\nHello world. Next.\n\nendif::[]\n"
    val conditioned: String = format(separated)
    assert(conditioned.contains("ifdef::x[]"))
    assert(conditioned.contains("endif::[]"))
    assert(conditioned.contains("Hello world.\nNext."))
    assert(structure(include) == structure(included))
  }

  test("header, comments, titles, and attributes stay") {
    val source: String =
      """= Title
        |Author
        |:attr: value
        |
        |// comment
        |Hello world. Next.
        |
        |.Title
        |A figure line.
        |
        |[NOTE]
        |====
        |inside
        |====
        |""".stripMargin
    val once: String = format(source)
    assert(once.startsWith("= Title\n"))
    assert(once.contains(":attr: value"))
    assert(once.contains("// comment"))
    assert(once.contains("Hello world.\nNext."))
    assert(once.contains(".Title"))
    assert(structure(source) == structure(once))
  }

  test("listings, verse, hard breaks, and plus lines stay") {
    val source: String =
      """Hello world. Next.
        |
        |----
        |code
        |----
        |
        |[verse]
        |____
        |line one
        |line two
        |____
        |
        |[%hardbreaks]
        |one
        |two
        |
        |line one +
        |line two
        |
        |* item text
        |+
        |extra block sentence.
        |""".stripMargin
    val once: String = format(source)
    assert(once.contains("----\ncode\n----"))
    assert(once.contains("line one\nline two"))
    assert(once.contains("[%hardbreaks]"))
    assert(once.contains("line one +\nline two"))
    assert(once.contains("+\n"))
    assert(once.contains("Hello world.\nNext."))
    val table: String = "| a | b |\n| --- | --- |\n| c | d |\n"
    assert(format(table) == table)
    assert(structure(source) == structure(once))
  }

  test("note, list, description, and macros") {
    val note: String = "NOTE: Hello world. * Next stays.\n"
    val noted: String = format(note)
    assert(noted.startsWith("NOTE: Hello world."))
    assert(noted.contains("* Next") || noted.contains("Hello world. *"))
    val list: String = "* Hello world. Next sentence.\n"
    val listed: String = format(list)
    assert(listed.startsWith("* Hello world.\n"))
    assert(listed.contains("  Next sentence."))
    val term: String = "Термин:: описание. Ещё.\n"
    val termed: String = format(term)
    assert(termed.startsWith("Термин::"))
    assert(!termed.substring(0, "Термин::".length).contains("о"))
    val macros: String = "See footnote:[One sentence. Two sentence.] and xref:a.adoc[here] and cite:[smith] and {attr} and `code` now.\n"
    val macroOut: String = format(macros)
    assert(macroOut.contains("xref:a.adoc[here]"))
    assert(macroOut.contains("cite:[smith]"))
    assert(macroOut.contains("{attr}"))
    assert(macroOut.contains("`code`"))
    assert(macroOut.contains("footnote:[One sentence."))
    assert(structure(note) == structure(noted))
    assert(format(format(macros)) == macroOut)
  }

  test("an empty description is left alone and a real one reflows") {
    // Three trailing spaces on Spaced::. stripMargin keeps them.
    val source: String =
      """= Terms
        |
        |[glossary]
        |[[Aba-Chonon]]Aba Chonon::
        |[[ach]][.hebrew]#אַךְ#:: but, only. Next sentence.
        |[[Achashverosh]]Achashverosh::
        |Bare::
        |Plain:: Hello world. Next sentence.
        |Role:: See [.hebrew]#אַךְ בְּ# today. Next sentence.
        |Term:: See note. NOTE: stay here.
        |Spaced::   
        |""".stripMargin
    val once: String = format(source)
    val expected: String =
      "= Terms\n\n[glossary]\n" +
        "[[Aba-Chonon]]Aba Chonon::\n" +
        "[[ach]][.hebrew]#אַךְ#:: but, only.\n" +
        (" " * 25) + "Next sentence.\n" +
        "[[Achashverosh]]Achashverosh::\n" +
        "Bare::\n" +
        "Plain:: Hello world.\n" +
        (" " * 8) + "Next sentence.\n" +
        "Role:: See [.hebrew]#אַךְ בְּ# today.\n" +
        (" " * 7) + "Next sentence.\n" +
        "Term:: See note. NOTE: stay here.\n" +
        "Spaced::   \n"
    assert(once == expected)
    assert(format(once) == once)
    assert(structure(source) == structure(once))

    val anchored: String = "[[ach]][.hebrew]#אַךְ#::\n"
    assert(format(anchored) == anchored)
    val named: String = "[[Aba]]Aba::\n"
    assert(format(named) == named)
    assert(format("Term:::\n") == "Term:::\n")
    assert(format("Term::::\n") == "Term::::\n")
    assert(format("Term;;\n") == "Term;;\n")
    val nbsp: String = "Term::\n\u00a0\n"
    assert(format(nbsp) == nbsp)

    val narrowOptions: ProseOptions = ProseOptions(width = 20, sentencePerLine = false)
    val narrowSource: String = "Plain:: Hello world next words here.\n"
    val narrow: String = AsciiDocProse.format(narrowSource, narrowOptions) match
      case Right(text) => text
      case Left(failure) => fail(failure.reason)
    val wrapped: String =
      "Plain:: Hello world\n" +
        (" " * 8) + "next words\n" +
        (" " * 8) + "here.\n"
    assert(narrow == wrapped)
    assert(AsciiDocProse.format(narrow, narrowOptions) == Right(narrow))

    val nested: String =
      """Outer:: intro. More.
        |+
        |--
        |Inner::
        |--
        |""".stripMargin
    val nestedOnce: String = format(nested)
    val nestedExpected: String =
      "Outer:: intro.\n" +
        (" " * 8) + "More.\n" +
        "+\n--\nInner::\n--\n"
    assert(nestedOnce == nestedExpected)
    assert(format(nestedOnce) == nestedOnce)
  }

  test("hebrew role text is kept when the span is broken") {
    val word = "[.hebrew]#בְּהַצְלָחָה#\n"
    val splitWord: String = format("[.hebrew]#בְּהַצְלָ\nחָה#\n")
    assert(splitWord == word)
    val splitPoint: String = format("[.hebrew]#בְּהַצ\nְלָחָה#\n")
    assert(splitPoint == word)
    val splitThree: String = format("[.hebrew]#בְּהַ\nצְלָ\nחָה#\n")
    assert(splitThree == word)
    assert(format(word) == word)
    val list: String = format("* See [.hebrew]#בְּהַצ\n  ְלָחָה#\n")
    assert(list.contains("[.hebrew]#בְּהַצְלָחָה#"))
    val sameLine: String = format("Good luck! [.hebrew]#בְּהַצְלָחָה#\n")
    assert(sameLine.contains("[.hebrew]#בְּהַצְלָחָה#"))
    val nextLine: String = format("Good luck!\n[.hebrew]#בְּהַצְלָחָה#\n")
    assert(nextLine.contains("[.hebrew]#בְּהַצְלָחָה#"))
    val unclosed: String = format("[.hebrew]#בְּהַצְלָחָה\n")
    assert(unclosed.contains("בְּהַצְלָחָה"))
    assert(unclosed.contains("[.hebrew]"))
    val dotted: String = format("Word [.hebrew]#בְּהַצְלָחָה#.\n")
    assert(dotted.contains("[.hebrew]#בְּהַצְלָחָה#"))
    val english: String = format("See [.hebrew]#two\nwords# today. Next.\n")
    assert(english.contains("[.hebrew]#two words#"))
    val spaced: String = "[.hebrew]#בְּשׁוֹגֵג בְּמֵזִיד#\n"
    assert(format(spaced) == spaced)
    val narrow: String = AsciiDocProse.format(
      "See [.hebrew]#בְּשׁוֹגֵג בְּמֵזִיד# today.\n",
      ProseOptions(width = 12, sentencePerLine = false)
    ) match
      case Right(text) => text
      case Left(failure) => fail(failure.reason)
    assert(narrow.contains("בְּשׁוֹגֵג בְּמֵזִיד"))
  }
