package org.podval.tools.publish.markup

import org.podval.xml.{HtmlXmlDialect, Xml, XmlParser}
import org.scalatest.funsuite.AnyFunSuite
import zio.blocks.chunk.Chunk

final class GlossarySpec extends AnyFunSuite:
  private def parse(input: String): Xml.Element =
    XmlParser.parseXml(input).toOption.get

  private def cleanup(input: String): Xml.Element =
    AsciiDocMarkup.cleanup(parse(input))

  private def render(element: Xml.Element): String =
    HtmlXmlDialect.render(element)

  test("definitions are taken from glossary dlist-item dd, not empty or non-glossary lists") {
    val xml: Xml.Element = cleanup(
      """<div>
        |<div class="dlist glossary">
        |<dl>
        |<dt><a id="posuk"></a>posuk</dt>
        |<dd><p>verse</p></dd>
        |<dt><a id="akdamus"></a>akdamus</dt>
        |<dt><a id="rasha"></a>rasha</dt>
        |<dd><p>sinner</p></dd>
        |</dl>
        |</div>
        |<div class="dlist">
        |<dl>
        |<dt><a id="cpu"></a>CPU</dt>
        |<dd><p>processor</p></dd>
        |</dl>
        |</div>
        |</div>
        |""".stripMargin
    )
    val defs: Map[String, Xml.Nodes] = Glossary.definitions(xml, HtmlXmlDialect)
    assert(defs.keySet == Set("posuk", "rasha"))
    assert(Xml.toString(defs("posuk")).trim == "verse")
    assert(Xml.toString(defs("rasha")).trim == "sinner")
  }

  test("attachTip and wrapRef produce a sibling tooltip outside the link") {
    val link: Xml.Element = Xml
      .element("a")
      .setId("src")
      .setHref("#posuk")
      .setText("posuk")
    val withTip: Xml.Element = Glossary.attachTip(link, Chunk(Xml.text("verse")))
    val wrapped: Xml.Element = parse("<p></p>").setChildren(Glossary.wrapRef(withTip).get)
    val rendered: String = render(wrapped)
    assert(rendered.contains("""class="glossary-ref""""))
    assert(rendered.contains("""class="glossary-tip""""))
    assert(rendered.contains("""id="src-tip""""))
    assert(rendered.contains("""aria-describedby="src-tip""""))
    assert(rendered.contains("""role="tooltip""""))
    assert(rendered.contains(">posuk</a>"))
    assert(rendered.contains("verse"))
    assert(!rendered.contains("<a href=\"#posuk\">posuk<span"))
  }
