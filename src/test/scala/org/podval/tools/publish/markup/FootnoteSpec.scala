package org.podval.tools.publish.markup

import org.podval.xml.{HtmlXmlDialect, Xml}
import org.scalatest.funsuite.AnyFunSuite
import zio.blocks.chunk.Chunk

final class FootnoteSpec extends AnyFunSuite:
  private def render(element: Xml.Element): String =
    HtmlXmlDialect.render(element)

  private def published(xml: Xml.Element, width: Int = 40): String =
    val (notes, harvested) = Footnote.harvest(xml)
    val resolved: Xml.Element = harvested.transform(el => Footnote.resolveLink(el, notes, attachTip = true))
    HtmlXmlDialect.render(resolved, width)

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
    val (notes, stripped) = Footnote.harvest(xml)
    assert(notes("a").number == 1)
    assert(notes("b").number == 2)
    assert(notes("a").nodes.map(_.getText).mkString == "first")
    assert(stripped.getChildren.flatMap(_.asElement).forall(!Footnote.isBody(_)))
    assert(Footnote.linkIds(stripped).toSeq == Seq("a", "b"))
  }

  test("unwrapLeftovers replaces matching containers with IR bodies") {
    val leftover: Xml.Element = Xml.element("div").addClass("footnotes").setChildren(Chunk(
      Xml.element("hr"),
      Xml.element("ol").setChildren(Chunk(
        Footnote.body("a", Chunk(Xml.text("hello")))
      ))
    ))
    val xml: Xml.Element = Xml.element("div").setChildren(Chunk(Footnote.link("a"), leftover))
    val unwrapped: Xml.Element = Footnote.unwrapLeftovers(
      xml,
      el => el.getName == "div" && el.hasClass("footnotes")
    )
    val dumped: String = render(unwrapped)
    assert(!dumped.contains("""class="footnotes""""), dumped)
    assert(!dumped.contains("<ol"), dumped)
    val bodies: Seq[Xml.Element] = unwrapped.gather(el => Option.when(Footnote.isBody(el))(el)).toSeq
    assert(bodies.size == 1, dumped)
    assert(bodies.head.getText == "hello", dumped)
  }

  test("appendReferenced adds only footnotes linked in the selected tree") {
    val xml: Xml.Element = Xml.element("div").setChildren(Chunk(
      Footnote.link("a"),
      Footnote.link("b"),
      Footnote.body("a", Chunk(Xml.text("first"))),
      Footnote.body("b", Chunk(Xml.text("second")))
    ))
    val (notes, stripped) = Footnote.harvest(xml)
    val onlyA: Xml.Element = stripped.setChildren(stripped.getChildren.take(1))
    val appended: Xml.Element = Footnote.appendReferenced(onlyA, notes)
    val dumped: String = render(appended)
    assert(dumped.contains("""class="footnotes""""), dumped)
    assert(dumped.contains("first"), dumped)
    assert(!dumped.contains("second"), dumped)
    assert(Footnote.appendReferenced(Xml.element("div"), notes).getChildren.isEmpty)
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

  test("footnote after text or a preceding element has no separating HTML space") {
    val afterText: Xml.Element = Xml.element("p").setChildren(Chunk(
      Xml.text("this"),
      Footnote.link("n"),
      Footnote.body("n", Chunk(Xml.text("a note"))),
      Xml.text(".")
    ))
    val afterEm: Xml.Element = Xml.element("p").setChildren(Chunk(
      Xml.element("em").setText("this"),
      Footnote.link("n"),
      Footnote.body("n", Chunk(Xml.text("a note"))),
      Xml.text(".")
    ))
    val afterA: Xml.Element = Xml.element("p").setChildren(Chunk(
      Xml.element("a").setHref("#x").setText("this"),
      Footnote.link("n"),
      Footnote.body("n", Chunk(Xml.text("a note"))),
      Xml.text(".")
    ))
    for (xml, before) <- Seq(
      afterText -> "this",
      afterEm -> "</em>",
      afterA -> "</a>"
    ) do
      val dumped: String = published(xml)
      val compact: String = dumped.replaceAll("\\s+", " ").replace("= ", "=")
      assert(compact.contains(s"""$before<span class="footnote-ref""""), dumped)
      assert(!compact.contains(s"""$before <span class="footnote-ref""""), dumped)
  }
