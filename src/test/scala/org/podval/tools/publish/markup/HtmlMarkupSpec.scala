package org.podval.tools.publish.markup

import org.podval.xml.{HtmlXmlDialect, Xml, XmlParser}
import org.scalatest.funsuite.AnyFunSuite
import zio.blocks.chunk.Chunk

final class HtmlMarkupSpec extends AnyFunSuite:
  test("nestSections wraps headings and transplants id off the header") {
    val xml: Xml.Element = XmlParser.parseXml(
      """<div><h2 id="colophon">Colophon</h2><p>body</p></div>"""
    ).toOption.get
    val nested: Xml.Element = xml.setChildren(HtmlMarkup.nestSections(xml.getChildren))
    val rendered: String = HtmlXmlDialect.render(nested)
    assert(rendered.contains("""class="section""""))
    assert(rendered.contains("""id="colophon""""))
    assert(rendered.contains("Colophon"))
    assert(!rendered.contains("""<h2 id="colophon""""))
    assert(!rendered.contains("""class="anchor""""))
  }

  test("addLinks wraps heading children in a self-link") {
    val header: Xml.Element = Xml.element("h2").setChildren(Chunk(Xml.text("Notes")))
    val rendered: String = HtmlXmlDialect.render(Section.addLinks(header, "notes"))
    assert(rendered.contains("""href="#notes""""))
    assert(rendered.contains("Notes"))
    assert(rendered.contains("""aria-hidden="true""""))
    assert(rendered.contains("""class="link""""))
    assert(rendered.contains("""class="heading""""))
  }

  test("addLinks does not nest anchors when the heading already contains a link") {
    val inner: Xml.Element = Xml.element("a").setHref("#term").setChildren(Chunk(Xml.text("Tisha B’Av")))
    val header: Xml.Element = Xml.element("h2").setChildren(Chunk(inner))
    val rendered: String = HtmlXmlDialect.render(Section.addLinks(header, "tisha-b-av"))
    assert(rendered.contains("""class="anchor""""))
    assert(rendered.contains("""href="#tisha-b-av""""))
    assert(!rendered.contains("""class="link""""))
    assert(rendered.contains("""href="#term""""))
  }
