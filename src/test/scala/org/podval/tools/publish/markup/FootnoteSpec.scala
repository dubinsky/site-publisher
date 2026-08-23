package org.podval.tools.publish.markup

import org.podval.xml.{HtmlXmlDialect, Xml, XmlParser}
import org.scalatest.funsuite.AnyFunSuite
import zio.blocks.chunk.Chunk

final class FootnoteSpec extends AnyFunSuite:
  private def parse(input: String): Xml.Element =
    XmlParser.parseXml(input).toOption.get

  private def render(element: Xml.Element): String =
    HtmlXmlDialect.render(element)

  test("attachTip and wrapRef produce a sibling tooltip outside the link") {
    val footnote = Footnote(
      correlationId = "1",
      number = 1,
      nodes = Chunk(Xml.text("a note"))
    )
    val withTip: Xml.Element = Footnote.tip.attachTip(footnote.link, footnote.nodes)
    val wrapped: Xml.Element = parse("<p></p>").setChildren(Footnote.tip.wrapRef(withTip).get)
    val rendered: String = render(wrapped).replaceAll("\\s+", " ").replace("= ", "=")
    assert(rendered.contains("""class="footnote-ref""""))
    assert(rendered.contains("""class="footnote-tip""""))
    assert(rendered.contains("""id="_footnote_src_1-tip""""))
    assert(rendered.contains("""aria-describedby="_footnote_src_1-tip""""))
    assert(rendered.contains("""role="tooltip""""))
    assert(rendered.contains("""href="#_footnote_1""""))
    assert(rendered.contains(">1</a>"))
    assert(rendered.contains("a note"))
    assert(rendered.contains(""">1</a><span class="footnote-tip""""))
  }

  test("wrapRef is empty when the link has no tip") {
    val footnote = Footnote(correlationId = "1", number = 1, nodes = Chunk(Xml.text("a note")))
    assert(Footnote.tip.wrapRef(footnote.link).isEmpty)
  }
