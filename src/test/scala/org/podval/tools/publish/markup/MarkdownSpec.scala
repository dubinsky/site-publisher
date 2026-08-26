package org.podval.tools.publish.markup

import org.podval.tools.publish.site.PageErrorReporter
import org.podval.xml.{HtmlXmlDialect, Xml, XmlParser}
import org.scalatest.funsuite.AnyFunSuite
import java.io.File

final class MarkdownSpec extends AnyFunSuite:
  private def parse(input: String): Xml.Element =
    XmlParser.parseXml(MarkdownMarkup.xmlContent(input, File("t.md"))).toOption.get

  private def process(source: String): Xml.Element =
    MarkdownMarkup.process(parse(source), PageErrorReporter.Silent)._1

  private def render(element: Xml.Element): String =
    HtmlXmlDialect.render(element)

  private def harvest(xml: Xml.Element): (Map[String, Footnote], Xml.Element) =
    Footnote.harvest(xml, MarkdownMarkup.isSpuriousFootnotesDiv)

  test("nested lists") {
    val xml: Xml.Element = parse(
      """* TOC
        |{:toc}
        |""".stripMargin
    )
    assert(xml.getName == "div")
  }

  test("See this [^note] becomes footnote IR; two uses share one body") {
    val xml: Xml.Element = process(
      """See this [^note] and again [^note].
        |
        |[^note]: A note.
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
    assert(Footnote.getCorrelationId(bodies.head) == ids.head, dumped)
    assert(dumped.contains("A note"), dumped)
    val (notes, stripped) = harvest(xml)
    assert(notes.size == 1)
    val noteNodes: String = render(Xml.element("span").setChildren(notes.values.head.nodes))
    assert(noteNodes.contains("A note"), noteNodes)
    assert(!noteNodes.contains("<p"), noteNodes)
    assert(!render(stripped).contains("""class="footnotes""""), render(stripped))
  }

  test("multi-paragraph FlexMark footnote unwraps each wrapping p") {
    val xml: Xml.Element = process(
      """See this [^note].
        |
        |[^note]: First paragraph.
        |
        |    Second paragraph.
        |""".stripMargin
    )
    val (notes, _) = harvest(xml)
    val noteNodes: String = render(Xml.element("span").setChildren(notes.values.head.nodes))
    assert(noteNodes.contains("First paragraph"), noteNodes)
    assert(noteNodes.contains("Second paragraph"), noteNodes)
    assert(!noteNodes.contains("<p"), noteNodes)
  }

  test("footnote after emphasis with no source space has no separating HTML space") {
    val xml: Xml.Element = process(
      """**this**[^note]
        |
        |[^note]: A note.
        |""".stripMargin
    )
    val (notes, harvested) = harvest(xml)
    val resolved: Xml.Element = harvested.transform(el => Footnote.resolveLink(el, notes, attachTip = true))
    val dumped: String = HtmlXmlDialect.render(resolved, 40)
    val compact: String = dumped.replaceAll("\\s+", " ").replace("= ", "=")
    assert(compact.contains("""</strong><span class="footnote-ref""""), dumped)
    assert(!compact.contains("""</strong> <span class="footnote-ref""""), dumped)
  }

  test("| A | B | table survives convert") {
    val xml: Xml.Element = process(
      """#| A | B |
        #|---|---|
        #| 1 | 2 |
        #""".stripMargin('#')
    )
    val dumped: String = render(xml)
    assert(dumped.contains("<table"), dumped)
    val cells: Seq[String] = xml.gather( element =>
      Option.when(element.getName == "th" || element.getName == "td")(element.getText.trim)
    ).toSeq.filter(_.nonEmpty)
    assert(cells.contains("A"), dumped)
    assert(cells.contains("B"), dumped)
    assert(cells.contains("1"), dumped)
    assert(cells.contains("2"), dumped)
  }

  test("fenced ```scala becomes language-scala; inline `map` is code") {
    val xml: Xml.Element = process(
      """Use `map` in
        |
        |```scala
        |xs.map(f)
        |```
        |""".stripMargin
    )
    val dumped: String = render(xml)
    assert(dumped.contains("""class="language-scala""""), dumped)
    assert(dumped.contains("xs.map(f)"), dumped)
    val codes: Seq[Xml.Element] = xml.gather( element =>
      Option.when(element.getName == "code")(element)
    ).toSeq
    val inline: Xml.Element = codes.find(c => !c.getClasses.exists(_.startsWith("language-"))).get
    assert(inline.getText.contains("map"), dumped)
    val pres: Seq[Xml.Element] = xml.gather( element =>
      Option.when(element.getName == "pre")(element)
    ).toSeq
    assert(pres.size == 1, dumped)
    val preCode: Xml.Element = pres.head.getChildren.flatMap(_.asElement).find(_.getName == "code").get
    assert(preCode.hasClass("language-scala"), dumped)
    assert(preCode.getText.contains("xs.map(f)"), dumped)
  }
