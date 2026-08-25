package org.podval.tools.publish.markup

import org.podval.xml.{HtmlXmlDialect, Xml}
import org.scalatest.funsuite.AnyFunSuite
import zio.blocks.chunk.Chunk

final class FootnoteSpec extends AnyFunSuite:
  private def render(element: Xml.Element): String =
    HtmlXmlDialect.render(element)

  test("attachTip wraps the link and tip as siblings") {
    val footnote = Footnote(
      correlationId = "1",
      number = 1,
      nodes = Chunk(Xml.text("a note"))
    )
    val withTip: Xml.Element = Footnote.tip.attachTip(footnote.link, footnote.nodes)
    val rendered: String = render(withTip).replaceAll("\\s+", " ").replace("= ", "=")
    assert(Footnote.tip.isRef(withTip))
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

  test("harvest numbers in link order and strips bodies") {
    val xml: Xml.Element = Xml.element("div").setChildren(Chunk(
      Footnote.body("b", Chunk(Xml.text("second"))),
      Footnote.link("a"),
      Footnote.link("b"),
      Footnote.body("a", Chunk(Xml.text("first")))
    ))
    val (notes, stripped) = Footnote.harvest(xml, HtmlXmlDialect)
    assert(notes("a").number == 1)
    assert(notes("b").number == 2)
    assert(notes("a").nodes.map(_.getText).mkString == "first")
    assert(stripped.getChildren.flatMap(_.asElement).forall(!Footnote.isBody(_)))
    assert(Footnote.linkIds(stripped, HtmlXmlDialect).toSeq == Seq("a", "b"))
  }

  test("harvest drops spurious footnote containers") {
    val inner: Xml.Element = Xml.element("div").addClass("footnotes").setChildren(Chunk(
      Footnote.body("a", Chunk(Xml.text("hello")))
    ))
    val xml: Xml.Element = Xml.element("div").setChildren(Chunk(Footnote.link("a"), inner))
    val (notes, stripped) = Footnote.harvest(
      xml,
      HtmlXmlDialect,
      isSpuriousFootnotesDiv = element => element.getName == "div" && element.hasClass("footnotes")
    )
    assert(notes("a").number == 1)
    val dumped: String = render(stripped)
    assert(!dumped.contains("""class="footnotes""""), dumped)
    assert(!dumped.contains("hello"), dumped)
  }

  test("appendReferenced adds only footnotes linked in the selected tree") {
    val xml: Xml.Element = Xml.element("div").setChildren(Chunk(
      Footnote.link("a"),
      Footnote.link("b"),
      Footnote.body("a", Chunk(Xml.text("first"))),
      Footnote.body("b", Chunk(Xml.text("second")))
    ))
    val (notes, stripped) = Footnote.harvest(xml, HtmlXmlDialect)
    val onlyA: Xml.Element = stripped.setChildren(stripped.getChildren.take(1))
    val appended: Xml.Element = Footnote.appendReferenced(onlyA, notes, HtmlXmlDialect)
    val dumped: String = render(appended)
    assert(dumped.contains("""class="footnotes""""), dumped)
    assert(dumped.contains("first"), dumped)
    assert(!dumped.contains("second"), dumped)
    assert(Footnote.appendReferenced(Xml.element("div"), notes, HtmlXmlDialect).getChildren.isEmpty)
  }

  test("resolveLink turns a stub into a numbered reference") {
    val footnote = Footnote(correlationId = "a", number = 2, nodes = Chunk(Xml.text("a note")))
    val notes: Map[String, Footnote] = Map("a" -> footnote)
    val resolved: Xml.Element = Footnote.resolveLink(Footnote.link("a"), notes, attachTip = false)
    val dumped: String = render(resolved)
    assert(resolved.getName == "a")
    assert(dumped.contains("""href="#_footnote_2""""), dumped)
    assert(dumped.contains(">2</a>") || dumped.contains(">2<"), dumped)
    val withTipEl: Xml.Element = Footnote.resolveLink(Footnote.link("a"), notes, attachTip = true)
    assert(Footnote.tip.isRef(withTipEl))
    val withTip: String = render(withTipEl)
    assert(withTip.contains("footnote-tip"), withTip)
    assert(Footnote.resolveLink(Xml.element("p"), notes, attachTip = true).getName == "p")
  }
