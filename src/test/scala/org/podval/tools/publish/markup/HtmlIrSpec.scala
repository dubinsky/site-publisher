package org.podval.tools.publish.markup

import org.podval.xml.{HtmlXmlWriterConfig, Xml, XmlParser}
import org.scalatest.funsuite.AnyFunSuite

final class HtmlIrSpec extends AnyFunSuite:
  private def parse(xml: String): Xml.Element = XmlParser.parseXml(xml).toOption.get

  private def render(element: Xml.Element): String = HtmlXmlWriterConfig.render(element)

  test("standalone img paragraph becomes a figure") {
    val xml: Xml.Element = HtmlIr.normalize(
      parse("""<div><p><img src="x.png" alt="a"/></p></div>""")
    )
    val dumped: String = render(xml)
    assert(xml.gather(el => Option.when(Figure.is(el))(el)).size == 1, dumped)
    assert(!dumped.contains("<p"), dumped)
  }

  test("blockquote gets class quote; s becomes del; aside gets class aside") {
    val xml: Xml.Element = HtmlIr.normalize(
      parse(
        """<div>
          |<blockquote><p>q</p></blockquote>
          |<p>See <s>old</s>.</p>
          |<aside><p>side</p></aside>
          |</div>"""
      )
    )
    val dumped: String = render(xml)
    assert(xml.gather(el => Option.when(Quote.is(el))(el)).size == 1, dumped)
    assert(xml.gather(el => Option.when(Strike.is(el))(el)).size == 1, dumped)
    assert(xml.gather(el => Option.when(Aside.is(el))(el)).size == 1, dumped)
    assert(dumped.contains("old"), dumped)
    assert(!dumped.contains("<s"), dumped)
  }

  test("pdf object becomes pdf-embed") {
    val xml: Xml.Element = HtmlIr.normalize(
      parse("""<div><object data="sample.pdf" type="application/pdf"></object></div>""")
    )
    val dumped: String = render(xml)
    assert(xml.gather(el => Option.when(PdfEmbed.is(el))(el)).size == 1, dumped)
    assert(dumped.contains("""class="pdf-embed-link""""), dumped)
  }
