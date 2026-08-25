package org.podval.tools.publish.markup

import org.asciidoctor.Asciidoctor
import org.podval.tools.publish.site.PageErrorReporter
import org.podval.xml.{HtmlXmlDialect, Xml, XmlParser}
import org.scalatest.funsuite.AnyFunSuite
import java.io.File

final class FigureSpec extends AnyFunSuite:
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

  private def figures(xml: Xml.Element): Seq[Xml.Element] =
    xml.gather(element => Option.when(Figure.is(element))(element)).toSeq

  test("AsciiDoc image:: with title becomes figure IR") {
    val xml: Xml.Element = fromAsciiDoc(
      """.A figure caption
        |image::pixel.svg[A square]
        |""".stripMargin
    )
    val dumped: String = render(xml)
    val found: Seq[Xml.Element] = figures(xml)
    assert(found.size == 1, dumped)
    assert(found.head.getName == "figure", dumped)
    assert(dumped.contains("<img"), dumped)
    assert(dumped.contains("pixel.svg"), dumped)
    assert(dumped.contains("""class="figure-caption""""), dumped)
    assert(dumped.contains("A figure caption"), dumped)
    assert(!dumped.contains("imageblock"), dumped)
  }

  test("AsciiDoc image:: without title is still a figure") {
    val xml: Xml.Element = fromAsciiDoc("image::pixel.svg[A square]\n")
    val dumped: String = render(xml)
    assert(figures(xml).size == 1, dumped)
    assert(dumped.contains("pixel.svg"), dumped)
    assert(figures(xml).head.gather(el => Option.when(Figure.isCaption(el))(el)).isEmpty, dumped)
    assert(!dumped.contains("imageblock"), dumped)
  }

  test("AsciiDoc inline image is not a figure") {
    val xml: Xml.Element = fromAsciiDoc("See image:pixel.svg[A square] here.\n")
    val dumped: String = render(xml)
    assert(figures(xml).isEmpty, dumped)
    assert(dumped.contains("<img"), dumped)
    assert(dumped.contains("pixel.svg"), dumped)
  }

  test("Markdown block image with title becomes figure IR") {
    val xml: Xml.Element = fromMarkdown("![A square](pixel.svg \"A Markdown figure\")\n")
    val dumped: String = render(xml)
    val found: Seq[Xml.Element] = figures(xml)
    assert(found.size == 1, dumped)
    assert(dumped.contains("pixel.svg"), dumped)
    assert(dumped.contains("""class="figure-caption""""), dumped)
    assert(dumped.contains("A Markdown figure"), dumped)
    assert(!dumped.contains("""title="A Markdown figure""""), dumped)
    assert(!dumped.contains("<p"), dumped)
  }

  test("Markdown block image without title is still a figure") {
    val xml: Xml.Element = fromMarkdown("![A square](pixel.svg)\n")
    val dumped: String = render(xml)
    assert(figures(xml).size == 1, dumped)
    assert(dumped.contains("pixel.svg"), dumped)
    assert(figures(xml).head.gather(el => Option.when(Figure.isCaption(el))(el)).isEmpty, dumped)
  }

  test("Markdown inline image is not a figure") {
    val xml: Xml.Element = fromMarkdown("See ![A square](pixel.svg) here.\n")
    val dumped: String = render(xml)
    assert(figures(xml).isEmpty, dumped)
    assert(dumped.contains("<img"), dumped)
    assert(dumped.contains("pixel.svg"), dumped)
  }

  test("HTML figure without class gets class figure") {
    val xml: Xml.Element = HtmlMarkup.process(
      parse("""<div><figure><img src="pixel.svg" alt="A square"/><figcaption>HTML figure</figcaption></figure></div>"""),
      PageErrorReporter.Silent
    )._1
    val dumped: String = render(xml)
    assert(figures(xml).size == 1, dumped)
    assert(dumped.contains("""class="figure""""), dumped)
    assert(dumped.contains("""class="figure-caption""""), dumped)
    assert(dumped.contains("HTML figure"), dumped)
  }

  test("HTML that is already IR is unchanged by HtmlMarkup.process") {
    val ir: Xml.Element = parse(
      """<div><figure class="figure"><img src="pixel.svg" alt="A square"/>
        |<figcaption class="figure-caption">already</figcaption></figure></div>"""
    )
    val processed: Xml.Element = HtmlMarkup.process(ir, PageErrorReporter.Silent)._1
    val dumped: String = render(processed)
    assert(figures(processed).size == 1, dumped)
    assert(dumped.contains("already"), dumped)
    assert(dumped.contains("pixel.svg"), dumped)
  }

  test("TEI figure with graphic and head becomes figure IR; graphic is img") {
    val xml: Xml.Element = fromTei(
      """<div>
        |<figure xml:id="fig1">
        |  <head>A TEI figure</head>
        |  <graphic url="pixel.svg"/>
        |</figure>
        |</div>""".stripMargin
    )
    val dumped: String = render(xml)
    val found: Seq[Xml.Element] = figures(xml)
    assert(found.size == 1, dumped)
    assert(found.head.getId.contains("fig1"), dumped)
    assert(dumped.contains("<img"), dumped)
    assert(dumped.contains("pixel.svg"), dumped)
    assert(dumped.contains("A TEI figure"), dumped)
    assert(dumped.contains("""class="figure-caption""""), dumped)
    assert(!dumped.contains("<image"), dumped)
    assert(!dumped.contains("<graphic"), dumped)
    assert(!dumped.contains("<tei-head"), dumped)
  }

  test("TEI graphic outside figure is still img") {
    val xml: Xml.Element = fromTei("""<div><p>See <graphic url="pixel.svg"/>.</p></div>""")
    val dumped: String = render(xml)
    assert(figures(xml).isEmpty, dumped)
    assert(dumped.contains("<img"), dumped)
    assert(dumped.contains("pixel.svg"), dumped)
    assert(!dumped.contains("<image"), dumped)
  }
