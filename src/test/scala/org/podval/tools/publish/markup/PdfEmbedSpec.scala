package org.podval.tools.publish.markup

import org.podval.tools.publish.site.PageErrorReporter
import org.podval.xml.{HtmlXmlWriterConfig, Xml, XmlParser}
import org.scalatest.funsuite.AnyFunSuite
import java.io.File

final class PdfEmbedSpec extends AnyFunSuite:
  private def render(element: Xml.Element): String = HtmlXmlWriterConfig.render(element)

  private def parse(xml: String): Xml.Element = XmlParser.parseXml(xml).toOption.get

  private def fromMarkdown(source: String): Xml.Element =
    MarkdownMarkup.process(
      parse(MarkdownMarkup.xmlContent(source, File("t.md"))),
      PageErrorReporter.Silent
    )._1

  private def embeds(xml: Xml.Element): Seq[Xml.Element] =
    xml.gather(element => Option.when(PdfEmbed.is(element))(element)).toSeq

  test("fromRef with page and height") {
    val xml: Xml.Element = PdfEmbed.fromRef("sample.pdf#page=2&height=480", "Handout")
    val dumped: String = render(xml)
    assert(PdfEmbed.is(xml), dumped)
    assert(dumped.contains("""data="sample.pdf#page=2""""), dumped)
    assert(dumped.contains("""type="application/pdf""""), dumped)
    assert(dumped.contains("""class="pdf-embed-link""""), dumped)
    assert(dumped.contains("Open PDF: Handout"), dumped)
    assert(dumped.contains("--pdf-embed-height: 480px"), dumped)
    val objects: Seq[Xml.Element] = xml.gather(el => Option.when(el.getName == "object")(el)).toSeq
    assert(objects.size == 1, dumped)
    assert(objects.head.gather(el => Option.when(el.getName == "a")(el)).nonEmpty, dumped)
  }

  test("WikiLink.embed of a pdf transclusion") {
    val a: Xml.Element = Xml
      .element("a")
      .addClass("wiki-link")
      .addClass("transclude")
      .setHref("sample.pdf#page=3")
      .setText("![[sample.pdf#page=3]]")
    val embedded: Xml.Element = WikiLink.embed(a, "sample.pdf#page=3").get
    val dumped: String = render(embedded)
    assert(PdfEmbed.is(embedded), dumped)
    assert(dumped.contains("""data="sample.pdf#page=3""""), dumped)
    assert(dumped.contains("sample.pdf"), dumped)
  }

  test("WikiLink.embed uses alias as label") {
    val a: Xml.Element = Xml
      .element("a")
      .addClass("wiki-link")
      .addClass("transclude")
      .setHref("sample.pdf")
      .setText("![[Handout]]")
    val embedded: Xml.Element = WikiLink.embed(a, "sample.pdf").get
    val dumped: String = render(embedded)
    assert(dumped.contains("Open PDF: Handout"), dumped)
  }

  test("HTML object becomes pdf-embed IR") {
    val xml: Xml.Element = HtmlIr.normalize(
      parse("""<div><object data="sample.pdf" type="application/pdf"></object></div>""")
    )
    val dumped: String = render(xml)
    assert(embeds(xml).size == 1, dumped)
    assert(dumped.contains("""class="pdf-embed-link""""), dumped)
    assert(dumped.contains("sample.pdf"), dumped)
  }

  test("HTML that is already IR is unchanged") {
    val ir: Xml.Element = PdfEmbed.make("sample.pdf", "already")
    val wrapped: Xml.Element = Xml.element("div").setChildren(zio.blocks.chunk.Chunk(ir))
    val processed: Xml.Element = HtmlIr.normalize(wrapped)
    val dumped: String = render(processed)
    assert(embeds(processed).size == 1, dumped)
    assert(dumped.contains("already"), dumped)
  }

  test("Markdown wiki embed stub is a transclude link until PageContent") {
    val xml: Xml.Element = fromMarkdown("See ![[sample.pdf]] here.\n")
    val dumped: String = render(xml)
    assert(embeds(xml).isEmpty, dumped)
    assert(dumped.contains("wiki-link"), dumped)
    assert(dumped.contains("transclude"), dumped)
    assert(dumped.contains("sample.pdf"), dumped)
  }
