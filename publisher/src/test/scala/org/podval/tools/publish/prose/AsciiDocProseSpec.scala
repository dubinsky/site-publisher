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
