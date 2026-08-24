package org.podval.tools.publish.markup

import org.podval.xml.{HtmlXmlDialect, Xml, XmlParser}
import org.scalatest.funsuite.AnyFunSuite
import zio.blocks.chunk.Chunk

final class HtmlMarkupSpec extends AnyFunSuite:
  test("section headers get a hover permalink from the section id") {
    val xml: Xml.Element = XmlParser.parseXml(
      """<div><h2 id="colophon">Colophon</h2><p>body</p></div>"""
    ).toOption.get
    val nested: Xml.Element = xml.setChildren(HtmlMarkup.nestSections(xml.getChildren))
    val rendered: String = HtmlXmlDialect.render(nested)
    assert(rendered.contains("""class="section""""))
    assert(rendered.contains("""id="colophon""""))
    assert(rendered.contains("""class="anchor""""))
    assert(rendered.contains("""href="#colophon""""))
    assert(rendered.contains("Colophon"))
    assert(!rendered.contains("""class="link""""))
    assert(!rendered.contains("""<h2 id="colophon""""))
  }

  test("addAnchor prepends a permalink and leaves heading text unlinked") {
    val header: Xml.Element = Xml.element("h2").setChildren(Chunk(Xml.text("Notes")))
    val rendered: String = HtmlXmlDialect.render(Section.addAnchor(header, "notes"))
    assert(rendered.contains("""href="#notes""""))
    assert(rendered.contains("Notes"))
    assert(rendered.contains("""aria-hidden="true""""))
    assert(!rendered.contains("""<a class="link""""))
  }
