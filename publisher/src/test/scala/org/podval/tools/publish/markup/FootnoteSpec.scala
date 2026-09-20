package org.podval.tools.publish.markup

import org.podval.tools.publish.site.{PageError, PageErrorReporter}
import org.podval.xml.{HtmlXmlWriterConfig, Xml, XmlElement, XmlParser}
import org.scalatest.funsuite.AnyFunSuite
import zio.blocks.chunk.Chunk

final class FootnoteSpec extends AnyFunSuite:
  private def render(element: Xml.Element): String =
    HtmlXmlWriterConfig.render(element)

  private def resolve(
    element: Xml.Element,
    combined: Map[String, Footnote],
    emitted: Map[String, Footnote] = Map.empty,
    attachTip: Boolean = true,
    report: PageErrorReporter = PageErrorReporter.Silent
  ): Xml.Element =
    Footnote.resolveLink(
      element,
      combined,
      if emitted.isEmpty then combined else emitted,
      attachTip,
      report
    )

  private def published(xml: Xml.Element, width: Int = 40): String =
    val (notes, harvested) = Footnote.harvest(xml)
    val resolved: Xml.Element = harvested.transform(el => resolve(el, notes))
    HtmlXmlWriterConfig.render(resolved, width)

  private final class RecordingReporter extends PageErrorReporter:
    var errors: Seq[(PageError.Kind, String)] = Seq.empty
    override def error(
      kind: PageError.Kind,
      message: String,
      cause: Option[Throwable] = None
    ): Unit =
      errors = errors :+ (kind -> message)

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
    val xml: Xml.Element = Xml.element(XmlElement.Div).setChildren(Chunk(
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
    val leftover: Xml.Element = Xml.element(XmlElement.Div).addClass("footnotes").setChildren(Chunk(
      Xml.element("hr"),
      Xml.element(XmlElement.Ol).setChildren(Chunk(
        Footnote.body("a", Chunk(Xml.text("hello")))
      ))
    ))
    val xml: Xml.Element = Xml.element(XmlElement.Div).setChildren(Chunk(Footnote.link("a"), leftover))
    val unwrapped: Xml.Element = Footnote.unwrapLeftovers(
      xml,
      el => el.isElement(XmlElement.Div) && el.hasClass("footnotes")
    )
    val dumped: String = render(unwrapped)
    assert(!dumped.contains("""class="footnotes""""), dumped)
    assert(!dumped.contains("<ol"), dumped)
    val bodies: Seq[Xml.Element] = unwrapped.gather(el => Option.when(Footnote.isBody(el))(el)).toSeq
    assert(bodies.size == 1, dumped)
    assert(bodies.head.getText == "hello", dumped)
  }

  test("appendReferenced adds only footnotes linked in the selected tree") {
    val xml: Xml.Element = Xml.element(XmlElement.Div).setChildren(Chunk(
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
    assert(Footnote.appendReferenced(Xml.element(XmlElement.Div), notes).getChildren.isEmpty)
  }

  test("resolveLink turns a stub into a numbered reference") {
    val footnote = Footnote(correlationId = "a", number = 2, nodes = Chunk(Xml.text("a note")))
    val notes: Map[String, Footnote] = Map("a" -> footnote)
    val resolved: Xml.Element = resolve(Footnote.link("a"), notes, attachTip = false)
    val dumped: String = render(resolved)
    assert(resolved.isA)
    assert(dumped.contains("""href="#_footnote_2""""), dumped)
    assert(dumped.contains(">2</a>") || dumped.contains(">2<"), dumped)
    val withTipEl: Xml.Element = resolve(Footnote.link("a"), notes, attachTip = true)
    assert(Footnote.tip.isRef(withTipEl))
    val withTip: String = render(withTipEl)
    assert(withTip.contains("footnote-tip"), withTip)
    assert(resolve(Xml.element(XmlElement.P), notes, attachTip = true).isElement(XmlElement.P))
  }

  test("harvest strips inner bodies from parent nodes and keeps inner stubs") {
    val xml: Xml.Element = Xml.element(XmlElement.Div).setChildren(Chunk(
      Footnote.link("outer"),
      Footnote.body("outer", Chunk(
        Xml.text("outer "),
        Footnote.link("inner"),
        Footnote.body("inner", Chunk(Xml.text("inner note")))
      ))
    ))
    val (notes, stripped) = Footnote.harvest(xml)
    assert(notes.contains("outer"))
    assert(notes.contains("inner"))
    val outerNodes: String = render(Xml.element(XmlElement.Span).setChildren(notes("outer").nodes))
    assert(outerNodes.contains("outer"), outerNodes)
    assert(Footnote.linkIds(Xml.element(XmlElement.Span).setChildren(notes("outer").nodes)).toSeq == Seq("inner"))
    assert(!notes("outer").nodes.exists(_.asElement.exists(Footnote.isBody)))
    assert(notes("inner").nodes.map(_.getText).mkString == "inner note")
    assert(Footnote.linkIds(stripped).toSeq == Seq("outer"))
    assert(stripped.getChildren.flatMap(_.asElement).forall(!Footnote.isBody(_)))
  }

  test("resolveLink missing from combined does not throw") {
    val reporter: RecordingReporter = RecordingReporter()
    val resolved: Xml.Element = Footnote.resolveLink(
      Footnote.link("missing"),
      Map.empty,
      Map.empty,
      attachTip = true,
      reporter
    )
    assert(Footnote.isLink(resolved))
    assert(resolved.hasClass("unresolved-link"))
    assert(reporter.errors.size == 1)
    assert(reporter.errors.head._1 eq PageError.UnknownFootnote)
    assert(reporter.errors.head._2.contains("missing"))
  }

  test("resolveLink in combined but not emitted leaves the stub with no error") {
    val footnote = Footnote(correlationId = "inner", number = 1, nodes = Chunk(Xml.text("inner note")))
    val combined: Map[String, Footnote] = Map("inner" -> footnote)
    val reporter: RecordingReporter = RecordingReporter()
    val resolved: Xml.Element = Footnote.resolveLink(
      Footnote.link("inner"),
      combined,
      Map.empty,
      attachTip = true,
      reporter
    )
    assert(Footnote.isLink(resolved))
    assert(!resolved.hasClass("unresolved-link"))
    assert(reporter.errors.isEmpty)
  }

  test("inner-only id is not OrphanFootnote; true orphan is") {
    val nested: Xml.Element = Xml.element(XmlElement.Div).setChildren(Chunk(
      Footnote.link("outer"),
      Footnote.body("outer", Chunk(
        Xml.text("outer "),
        Footnote.link("inner"),
        Footnote.body("inner", Chunk(Xml.text("inner note")))
      ))
    ))
    val (nestedNotes, nestedStripped) = Footnote.harvest(nested)
    val nestedReporter: RecordingReporter = RecordingReporter()
    Footnote.reportOrphans(nestedNotes, nestedStripped, nestedReporter)
    assert(!nestedReporter.errors.exists(_._1 eq PageError.OrphanFootnote), nestedReporter.errors)

    val orphanXml: Xml.Element = Xml.element(XmlElement.Div).setChildren(Chunk(
      Footnote.body("lonely", Chunk(Xml.text("nobody cites me")))
    ))
    val (orphanNotes, orphanStripped) = Footnote.harvest(orphanXml)
    val orphanReporter: RecordingReporter = RecordingReporter()
    Footnote.reportOrphans(orphanNotes, orphanStripped, orphanReporter)
    assert(orphanReporter.errors.size == 1)
    assert(orphanReporter.errors.head._1 eq PageError.OrphanFootnote)
    assert(orphanReporter.errors.head._2.contains("lonely"))
  }

  test("two running-text refs emit one body and two links") {
    val xml: Xml.Element = Xml.element(XmlElement.Div).setChildren(Chunk(
      Footnote.link("a"),
      Xml.text(" and "),
      Footnote.link("a"),
      Footnote.body("a", Chunk(Xml.text("shared")))
    ))
    val finished: Xml.Element = Footnote.finish(xml, PageErrorReporter.Silent)
    val dumped: String = render(finished)
    val links: Seq[Xml.Element] = finished.gather(el =>
      Option.when(el.hasClass("footnote-link"))(el)
    )
    assert(links.size == 2, dumped)
    val bodies: Seq[Xml.Element] = finished.gather(el =>
      Option.when(el.hasClass("footnote") && el.getId.contains("_footnote_1"))(el)
    )
    assert(bodies.size == 1, dumped)
    assert(dumped.indexOf("""id="_footnote_1"""") == dumped.lastIndexOf("""id="_footnote_1""""), dumped)
  }

  test("PageError.all contains UnknownFootnote and OrphanFootnote") {
    assert(PageError.all.contains(PageError.UnknownFootnote))
    assert(PageError.all.contains(PageError.OrphanFootnote))
    assert(PageError.UnknownFootnote.id == "unknown-footnote")
    assert(PageError.OrphanFootnote.id == "orphan-footnote")
  }

  test("finish does not nest footnote-tip inside a tip") {
    val xml: Xml.Element = Xml.element(XmlElement.Div).setChildren(Chunk(
      Footnote.link("outer"),
      Footnote.body("outer", Chunk(
        Xml.text("outer "),
        Footnote.link("inner"),
        Footnote.body("inner", Chunk(Xml.text("inner note")))
      ))
    ))
    val finished: Xml.Element = Footnote.finish(xml, PageErrorReporter.Silent)
    val dumped: String = render(finished)
    val tips: Seq[Xml.Element] = finished.gather(el => Option.when(el.hasClass("footnote-tip"))(el))
    assert(tips.nonEmpty, dumped)
    tips.foreach: tip =>
      val nested: Seq[Xml.Element] = tip.gather(el => Option.when(el.hasClass("footnote-tip"))(el))
      assert(nested.size == 1, render(tip))
      assert(nested.head eq tip, render(tip))
    val tei: Xml.Element = XmlParser.parseXml(
      """<p>See<note place="end">outer<note place="end">inner</note></note>.</p>"""
    ).toOption.get
    val fromTei: Xml.Element = TeiMarkup.finishFootnotes(tei, PageErrorReporter.Silent)
    val teiTips: Seq[Xml.Element] = fromTei.gather(el => Option.when(el.hasClass("footnote-tip"))(el))
    assert(teiTips.nonEmpty, render(fromTei))
    teiTips.foreach: tip =>
      val nested: Seq[Xml.Element] = tip.gather(el => Option.when(el.hasClass("footnote-tip"))(el))
      assert(nested.size == 1, render(tip))
      assert(nested.head eq tip, render(tip))
  }

  test("footnote after text or a preceding element has no separating HTML space") {
    val afterText: Xml.Element = Xml.element(XmlElement.P).setChildren(Chunk(
      Xml.text("this"),
      Footnote.link("n"),
      Footnote.body("n", Chunk(Xml.text("a note"))),
      Xml.text(".")
    ))
    val afterEm: Xml.Element = Xml.element(XmlElement.P).setChildren(Chunk(
      Xml.element(XmlElement.Em).setText("this"),
      Footnote.link("n"),
      Footnote.body("n", Chunk(Xml.text("a note"))),
      Xml.text(".")
    ))
    val afterA: Xml.Element = Xml.element(XmlElement.P).setChildren(Chunk(
      Xml.element(XmlElement.A).setHref("#x").setText("this"),
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
