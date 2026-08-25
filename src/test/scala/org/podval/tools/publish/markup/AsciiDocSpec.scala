package org.podval.tools.publish.markup

import org.asciidoctor.Asciidoctor
import org.podval.tools.publish.site.PageErrorReporter
import org.podval.xml.{HtmlXmlDialect, Xml, XmlParser}
import org.scalatest.funsuite.AnyFunSuite
import java.io.File

final class AsciiDocSpec extends AnyFunSuite:
  private lazy val asciidoctor: Asciidoctor =
    val result: Asciidoctor = Asciidoctor.Factory.create()
    AsciiDocCiteExtension.register(result)
    result

  private def cleanup(input: String): String =
    val parsed = XmlParser.parseXml(input).toOption.get
    HtmlXmlDialect.render(AsciiDocMarkup.cleanup(parsed))

  private def process(source: String): Xml.Element =
    AsciiDocMarkup.process(
      XmlParser.parseXml(AsciiDocMarkup.convert(source, File("t.adoc").getAbsoluteFile, asciidoctor)).toOption.get,
      PageErrorReporter.Silent
    )._1

  private def render(element: Xml.Element): String =
    HtmlXmlDialect.render(element)

  private def harvest(xml: Xml.Element): (Map[String, Footnote], Xml.Element) =
    Footnote.harvest(xml, AsciiDocMarkup.isSpuriousFootnotesDiv)

  private def isItem(rendered: String, id: String, cssClass: String): Boolean =
    rendered.contains(s"""<div class="$cssClass" id="$id">""") ||
    rendered.contains(s"""<div id="$id" class="$cssClass">""")

  test("glossary term ids are hoisted onto glossary-item and each term is grouped") {
    val rendered: String = cleanup(
      """<div class="dlist glossary">
        |<dl>
        |<dt><a id="posuk"></a>posuk</dt>
        |<dd><p>verse</p></dd>
        |<dt><a id="akdamus"></a>akdamus</dt>
        |<dt><a id="rasha"></a>rasha</dt>
        |<dd><p>sinner</p></dd>
        |</dl>
        |</div>
        |""".stripMargin
    )
    assert(isItem(rendered, "posuk", "glossary-item"))
    assert(isItem(rendered, "akdamus", "glossary-item"))
    assert(isItem(rendered, "rasha", "glossary-item"))
    assert(rendered.contains("""class="glossary""""))
    assert(!rendered.contains("dlist"))
    assert(!rendered.contains("dlist-item"))
    assert(rendered.contains("<dt>posuk</dt>"))
    assert(rendered.contains("<dt>akdamus</dt>"))
    assert(rendered.contains("<dt>rasha</dt>"))
    assert(rendered.contains("<dd>verse</dd>"))
    assert(rendered.contains("<dd>sinner</dd>"))
    assert(!rendered.contains("<dd><p>"))
    assert(!rendered.contains("""<a id="posuk">"""))
    assert(!rendered.contains("""<a id="akdamus">"""))
    assert(!rendered.contains("""<a id="rasha">"""))
    assert(!rendered.contains("""<dt id="posuk">"""))
    assert(!rendered.contains("""<dt id="rasha">"""))
  }

  test("non-glossary description lists are left as Asciidoctor emitted them") {
    val rendered: String = cleanup(
      """<div class="dlist">
        |<dl>
        |<dt><a id="cpu"></a>CPU</dt>
        |<dd><p>processor</p></dd>
        |</dl>
        |</div>
        |""".stripMargin
    )
    assert(rendered.contains("""<a id="cpu">"""))
    assert(rendered.contains("<dt>"))
    assert(rendered.contains("<dd>processor</dd>"))
    assert(!rendered.contains("glossary-item"))
    assert(!rendered.contains("dlist-item"))
    assert(!isItem(rendered, "cpu", "glossary-item"))
  }

  test("empty id anchors with href are left in place") {
    val rendered: String = cleanup(
      """<div>
        |<dl>
        |<dt><a id="stay" href="#other"></a>term</dt>
        |<dd><p>def</p></dd>
        |</dl>
        |</div>
        |""".stripMargin
    )
    assert(rendered.contains("""<a id="stay" href="#other">"""))
    assert(rendered.contains("<dd>def</dd>"))
    assert(!rendered.contains("<dd><p>"))
    assert(!rendered.contains("""<dt id="stay">"""))
    assert(!isItem(rendered, "stay", "glossary-item"))
  }

  test("footnote:[A note.] becomes footnote IR") {
    val xml: Xml.Element = process("See this footnote:[A note.]\n")
    val dumped: String = render(xml)
    val ids: Seq[String] = Footnote.linkIds(xml).toSeq
    assert(ids.size == 1, dumped)
    val bodies: Seq[Xml.Element] = xml.gather( element =>
      Option.when(Footnote.isBody(element))(element)
    ).toSeq
    assert(bodies.size == 1, dumped)
    assert(Footnote.getCorrelationId(bodies.head) == ids.head, dumped)
    assert(dumped.contains("A note"), dumped)
    assert(!dumped.contains("_footnoteref"), dumped)
    assert(!dumped.contains("_footnotedef"), dumped)
    val (notes, stripped) = harvest(xml)
    assert(notes.size == 1)
    assert(Xml.toString(notes.values.head.nodes).contains("A note"))
    assert(!render(stripped).contains("""id="footnotes""""), render(stripped))
  }

  test("footnote:fn[] reuses one body for two links") {
    val xml: Xml.Element = process(
      """See this footnote:fn[Same note.] later footnote:fn[].
        |""".stripMargin
    )
    val dumped: String = render(xml)
    val ids: Seq[String] = Footnote.linkIds(xml).toSeq
    assert(ids.size == 2, dumped)
    assert(ids.toSet.size == 1, dumped)
    val bodies: Seq[Xml.Element] = xml.gather( element =>
      Option.when(Footnote.isBody(element))(element)
    ).toSeq
    assert(bodies.size == 1, dumped)
    assert(dumped.contains("Same note"), dumped)
    val (notes, _) = harvest(xml)
    assert(notes.size == 1)
    assert(Xml.toString(notes.values.head.nodes).contains("Same note"))
  }

  test("|=== table survives cleanup without tableblock") {
    val xml: Xml.Element = process(
      """#|===
        #| A | B
        #
        #| 1 | 2
        #|===
        #""".stripMargin('#')
    )
    val dumped: String = render(xml)
    assert(dumped.contains("<table"), dumped)
    assert(!dumped.contains("tableblock"), dumped)
    val cells: Seq[String] = xml.gather( element =>
      Option.when(element.getName == "th" || element.getName == "td")(element.getText.trim)
    ).toSeq.filter(_.nonEmpty)
    assert(cells.contains("A"), dumped)
    assert(cells.contains("B"), dumped)
    assert(cells.contains("1"), dumped)
    assert(cells.contains("2"), dumped)
  }

  test("[source,scala] becomes language-scala") {
    val xml: Xml.Element = process(
      """[source,scala]
        |----
        |xs.map(f)
        |----
        |""".stripMargin
    )
    val dumped: String = render(xml)
    assert(dumped.contains("""class="language-scala""""), dumped)
    assert(dumped.contains("xs.map(f)"), dumped)
    val codes: Seq[Xml.Element] = xml.gather( element =>
      Option.when(element.getName == "code")(element)
    ).toSeq
    assert(codes.exists(_.hasClass("language-scala")), dumped)
  }
