package org.podval.tools.publish.markup

import org.asciidoctor.Asciidoctor
import org.podval.tools.publish.site.PageErrorReporter
import org.podval.xml.{HtmlXmlDialect, Xml, XmlParser}
import org.scalatest.funsuite.AnyFunSuite
import java.io.File

final class StrikeSpec extends AnyFunSuite:
  private lazy val asciidoctor: Asciidoctor =
    val result: Asciidoctor = Asciidoctor.Factory.create()
    AsciiDocCiteExtension.register(result)
    result

  private def render(element: Xml.Element): String = HtmlXmlDialect.render(element)

  private def parse(xml: String): Xml.Element = XmlParser.parseXml(xml).toOption.get

  private def fromAsciiDoc(source: String): Xml.Element =
    AsciiDocMarkup.process(
      parse(AsciiDocMarkup.convert(source, File("t.adoc").getAbsoluteFile, asciidoctor)),
      PageErrorReporter.Silent
    )._1

  private def fromMarkdown(source: String): Xml.Element =
    MarkdownMarkup.process(
      parse(MarkdownMarkup.xmlContent(source, File("t.md"))),
      PageErrorReporter.Silent
    )._1

  private def fromTei(source: String): Xml.Element =
    TeiMarkup.process(parse(source), PageErrorReporter.Silent)._1

  private def fromDocBook(source: String): Xml.Element =
    DocBookMarkup.process(parse(source), PageErrorReporter.Silent)._1

  private def strikes(xml: Xml.Element): Seq[Xml.Element] =
    xml.gather(element => Option.when(Strike.is(element))(element)).toSeq

  test("Markdown ~~text~~ becomes del") {
    val xml: Xml.Element = fromMarkdown("See ~~struck out~~ here.\n")
    val dumped: String = render(xml)
    val found: Seq[Xml.Element] = strikes(xml)
    assert(found.size == 1, dumped)
    assert(found.head.getName == "del", dumped)
    assert(dumped.contains("struck out"), dumped)
    assert(!dumped.contains("~~"), dumped)
  }

  test("AsciiDoc [line-through] becomes del") {
    val xml: Xml.Element = fromAsciiDoc("See [line-through]#obsolete phrase# here.\n")
    val dumped: String = render(xml)
    val found: Seq[Xml.Element] = strikes(xml)
    assert(found.size == 1, dumped)
    assert(dumped.contains("obsolete phrase"), dumped)
    assert(!dumped.contains("line-through"), dumped)
    assert(!dumped.contains("<mark"), dumped)
    assert(!dumped.contains("<span"), dumped)
  }

  test("HTML <s> becomes del") {
    val xml: Xml.Element = HtmlMarkup.process(
      parse("""<div><p>See <s>old name</s>.</p></div>"""),
      PageErrorReporter.Silent
    )._1
    val dumped: String = render(xml)
    assert(strikes(xml).size == 1, dumped)
    assert(dumped.contains("<del"), dumped)
    assert(dumped.contains("old name"), dumped)
    assert(!dumped.contains("<s"), dumped)
  }

  test("HTML span.line-through becomes del") {
    val xml: Xml.Element = HtmlMarkup.process(
      parse("""<div><p>See <span class="line-through">legacy span</span>.</p></div>"""),
      PageErrorReporter.Silent
    )._1
    val dumped: String = render(xml)
    assert(strikes(xml).size == 1, dumped)
    assert(dumped.contains("legacy span"), dumped)
    assert(!dumped.contains("line-through"), dumped)
  }

  test("HTML that is already del is unchanged") {
    val ir: Xml.Element = parse("""<div><p>See <del>HTML struck</del>.</p></div>""")
    val processed: Xml.Element = HtmlMarkup.process(ir, PageErrorReporter.Silent)._1
    val dumped: String = render(processed)
    assert(strikes(processed).size == 1, dumped)
    assert(dumped.contains("HTML struck"), dumped)
  }

  test("TEI del stays del") {
    val xml: Xml.Element = fromTei("""<div><p>See <del>TEI struck</del>.</p></div>""")
    val dumped: String = render(xml)
    assert(strikes(xml).size == 1, dumped)
    assert(dumped.contains("TEI struck"), dumped)
    assert(!dumped.contains("tei-class"), dumped)
  }

  test("DocBook emphasis role=strikethrough becomes del") {
    val xml: Xml.Element = fromDocBook(
      """<article><para>See <emphasis role="strikethrough">DocBook struck</emphasis>.</para></article>"""
    )
    val dumped: String = render(xml)
    assert(strikes(xml).size == 1, dumped)
    assert(dumped.contains("<del"), dumped)
    assert(dumped.contains("DocBook struck"), dumped)
    assert(!dumped.contains("<emphasis"), dumped)
    assert(!dumped.contains("db-class"), dumped)
  }

  test("Markdown ~~ inside code is not strikethrough") {
    val xml: Xml.Element = fromMarkdown("Use `~~literal~~`.\n")
    val dumped: String = render(xml)
    assert(strikes(xml).isEmpty, dumped)
    assert(dumped.contains("~~literal~~"), dumped)
  }
