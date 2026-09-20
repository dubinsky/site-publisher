package org.podval.tools.publish.markup

import org.podval.tools.publish.site.{PageError, PageErrorReporter}
import org.podval.xml.{HtmlXmlWriterConfig, Xml, XmlElement, XmlParser, XmlWriterConfig}
import org.scalatest.funsuite.AnyFunSuite
import zio.blocks.chunk.Chunk

final class FootnoteSpec extends AnyFunSuite:
  private def render(element: Xml.Element): String =
    (HtmlXmlWriterConfig: XmlWriterConfig).render(element)

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
    (HtmlXmlWriterConfig: XmlWriterConfig).render(resolved, width)

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
    val withTip: Xml.Element = Footnote.tip.attachTip(footnote.link(), footnote.nodes)
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
    val copyWithId: Xml.Element = Footnote.resolveLink(
      Footnote.link("a"),
      notes,
      notes,
      attachTip = false,
      PageErrorReporter.Silent,
      withId = true
    )
    val copyHtml: String = render(copyWithId)
    assert(copyHtml.contains("""id="_footnote_src_2""""), copyHtml)
    assert(!copyHtml.contains("footnote-tip"), copyHtml)
    assert(!dumped.contains("""id="_footnote_src_2""""), dumped)
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

  test("PageError.all contains UnknownFootnote, OrphanFootnote, FootnoteScopeConflict, FootnoteNesting, and FootnoteCycle") {
    assert(PageError.all.contains(PageError.UnknownFootnote))
    assert(PageError.all.contains(PageError.OrphanFootnote))
    assert(PageError.all.contains(PageError.FootnoteScopeConflict))
    assert(PageError.all.contains(PageError.FootnoteNesting))
    assert(PageError.all.contains(PageError.FootnoteCycle))
    assert(PageError.UnknownFootnote.id == "unknown-footnote")
    assert(PageError.OrphanFootnote.id == "orphan-footnote")
    assert(PageError.FootnoteScopeConflict.id == "footnote-scope-conflict")
    assert(PageError.FootnoteNesting.id == "footnote-nesting")
    assert(PageError.FootnoteCycle.id == "footnote-cycle")
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

  private def cellTable(nodes: Xml.Nodes): Xml.Element =
    Xml.element(XmlElement.Table).setChildren(Chunk(
      Xml.element(XmlElement.Tr).setChildren(Chunk(
        Xml.element(XmlElement.Td).setChildren(nodes)
      ))
    ))

  private def finishTables(
    xml: Xml.Element,
    report: PageErrorReporter = PageErrorReporter.Silent
  ): Xml.Element =
    Footnote.finish(xml, report, localTables = true)

  test("letterLabel is a, z, aa, ab") {
    assert(Footnote.letterLabel(0) == "a")
    assert(Footnote.letterLabel(25) == "z")
    assert(Footnote.letterLabel(26) == "aa")
    assert(Footnote.letterLabel(27) == "ab")
  }

  test("table wrap uses letters; two cells sharing a note emit one body") {
    val xml: Xml.Element = Xml.element(XmlElement.Div).setChildren(Chunk(
      Xml.element(XmlElement.Table).setChildren(Chunk(
        Xml.element(XmlElement.Tr).setChildren(Chunk(
          Xml.element(XmlElement.Td).setChildren(Chunk(Footnote.link("a"))),
          Xml.element(XmlElement.Td).setChildren(Chunk(Footnote.link("a")))
        ))
      )),
      Footnote.body("a", Chunk(Xml.text("shared cell")))
    ))
    val finished: Xml.Element = finishTables(xml)
    val dumped: String = render(finished)
    val wrappers: Seq[Xml.Element] = finished.gather(el =>
      Option.when(el.hasClass("table-with-notes"))(el)
    )
    assert(wrappers.size == 1, dumped)
    val tableLists: Seq[Xml.Element] = finished.gather(el =>
      Option.when(el.hasClass("table-footnotes"))(el)
    )
    assert(tableLists.size == 1, dumped)
    val bodies: Seq[Xml.Element] = finished.gather(el =>
      Option.when(el.getId.contains("_table_1_fn_a"))(el)
    )
    assert(bodies.size == 1, dumped)
    assert(dumped.contains("""data-footnote-scope="table""""), dumped)
    assert(dumped.contains("shared cell"), dumped)
    val links: Seq[Xml.Element] = finished.gather(el =>
      Option.when(el.hasClass("footnote-link"))(el)
    )
    assert(links.size == 2, dumped)
    assert(links.forall(_.getText == "a"), dumped)
    assert(!dumped.contains("""id="_footnote_1""""), dumped)
  }

  test("two ids in one table are a then b") {
    val xml: Xml.Element = Xml.element(XmlElement.Div).setChildren(Chunk(
      Xml.element(XmlElement.Table).setChildren(Chunk(
        Xml.element(XmlElement.Tr).setChildren(Chunk(
          Xml.element(XmlElement.Td).setChildren(Chunk(Footnote.link("x"))),
          Xml.element(XmlElement.Td).setChildren(Chunk(Footnote.link("y")))
        ))
      )),
      Footnote.body("x", Chunk(Xml.text("letter a"))),
      Footnote.body("y", Chunk(Xml.text("letter b")))
    ))
    val finished: Xml.Element = finishTables(xml)
    val dumped: String = render(finished)
    assert(dumped.contains("""id="_table_1_fn_a""""), dumped)
    assert(dumped.contains("""id="_table_1_fn_b""""), dumped)
    assert(dumped.contains("letter a"), dumped)
    assert(dumped.contains("letter b"), dumped)
    assert(!dumped.contains("_table_2"), dumped)
    val wrappers: Seq[Xml.Element] = finished.gather(el =>
      Option.when(el.hasClass("table-with-notes"))(el)
    )
    assert(wrappers.size == 1, dumped)
  }

  test("two tables restart letters at a") {
    val xml: Xml.Element = Xml.element(XmlElement.Div).setChildren(Chunk(
      cellTable(Chunk(Footnote.link("x"))),
      cellTable(Chunk(Footnote.link("y"))),
      Footnote.body("x", Chunk(Xml.text("first table"))),
      Footnote.body("y", Chunk(Xml.text("second table")))
    ))
    val finished: Xml.Element = finishTables(xml)
    val dumped: String = render(finished)
    assert(dumped.contains("""id="_table_1_fn_a""""), dumped)
    assert(dumped.contains("""id="_table_2_fn_a""""), dumped)
    assert(dumped.contains("first table"), dumped)
    assert(dumped.contains("second table"), dumped)
    val wrappers: Seq[Xml.Element] = finished.gather(el =>
      Option.when(el.hasClass("table-with-notes"))(el)
    )
    assert(wrappers.size == 2, dumped)
  }

  test("document and table notes use split series") {
    val xml: Xml.Element = Xml.element(XmlElement.Div).setChildren(Chunk(
      Footnote.link("doc"),
      cellTable(Chunk(Footnote.link("cell"))),
      Footnote.body("doc", Chunk(Xml.text("running"))),
      Footnote.body("cell", Chunk(Xml.text("under table")))
    ))
    val finished: Xml.Element = finishTables(xml)
    val dumped: String = render(finished)
    assert(dumped.contains("table-with-notes"), dumped)
    assert(dumped.contains("""id="_table_1_fn_a""""), dumped)
    assert(dumped.contains("under table"), dumped)
    assert(dumped.contains("""id="_footnote_1""""), dumped)
    assert(dumped.contains("running"), dumped)
    val pageLists: Seq[Xml.Element] = finished.gather(el =>
      Option.when(el.hasClass("footnotes") && !el.hasClass("table-footnotes"))(el)
    )
    assert(pageLists.size == 1, dumped)
    assert(pageLists.head.getText.contains("running"), render(pageLists.head))
    assert(!pageLists.head.getText.contains("under table"), render(pageLists.head))
  }

  test("same id in table and running text is FootnoteScopeConflict and stays arabic") {
    val xml: Xml.Element = Xml.element(XmlElement.Div).setChildren(Chunk(
      Footnote.link("a"),
      cellTable(Chunk(Footnote.link("a"))),
      Footnote.body("a", Chunk(Xml.text("conflicted")))
    ))
    val reporter: RecordingReporter = RecordingReporter()
    val finished: Xml.Element = finishTables(xml, reporter)
    val dumped: String = render(finished)
    assert(reporter.errors.exists(_._1 eq PageError.FootnoteScopeConflict), reporter.errors)
    assert(!dumped.contains("table-with-notes"), dumped)
    assert(dumped.contains("""id="_footnote_1""""), dumped)
    assert(!dumped.contains("_table_"), dumped)
  }

  test("same id in nested tables is FootnoteScopeConflict") {
    val inner: Xml.Element = cellTable(Chunk(Footnote.link("a")))
    val xml: Xml.Element = Xml.element(XmlElement.Div).setChildren(Chunk(
      Xml.element(XmlElement.Table).setChildren(Chunk(
        Xml.element(XmlElement.Tr).setChildren(Chunk(
          Xml.element(XmlElement.Td).setChildren(Chunk(Footnote.link("a"), inner))
        ))
      )),
      Footnote.body("a", Chunk(Xml.text("nested conflict")))
    ))
    val reporter: RecordingReporter = RecordingReporter()
    val finished: Xml.Element = finishTables(xml, reporter)
    val dumped: String = render(finished)
    assert(reporter.errors.exists(_._1 eq PageError.FootnoteScopeConflict), reporter.errors)
    assert(!dumped.contains("table-with-notes"), dumped)
    assert(dumped.contains("""id="_footnote_1""""), dumped)
  }

  test("conflicted table does not consume k") {
    val xml: Xml.Element = Xml.element(XmlElement.Div).setChildren(Chunk(
      Footnote.link("conflict"),
      cellTable(Chunk(Footnote.link("conflict"))),
      cellTable(Chunk(Footnote.link("clean"))),
      Footnote.body("conflict", Chunk(Xml.text("arabic"))),
      Footnote.body("clean", Chunk(Xml.text("letter")))
    ))
    val reporter: RecordingReporter = RecordingReporter()
    val finished: Xml.Element = finishTables(xml, reporter)
    val dumped: String = render(finished)
    assert(reporter.errors.exists(_._1 eq PageError.FootnoteScopeConflict), reporter.errors)
    assert(dumped.contains("""id="_footnote_1""""), dumped)
    assert(dumped.contains("arabic"), dumped)
    assert(dumped.contains("""id="_table_1_fn_a""""), dumped)
    assert(dumped.contains("letter"), dumped)
    assert(!dumped.contains("_table_2"), dumped)
    val wrappers: Seq[Xml.Element] = finished.gather(el =>
      Option.when(el.hasClass("table-with-notes"))(el)
    )
    assert(wrappers.size == 1, dumped)
  }

  test("finish with localTables false keeps a layout table arabic") {
    val xml: Xml.Element = Xml.element(XmlElement.Div).setChildren(Chunk(
      cellTable(Chunk(Footnote.link("a"))),
      Footnote.body("a", Chunk(Xml.text("chrome")))
    ))
    val finished: Xml.Element = Footnote.finish(xml, PageErrorReporter.Silent)
    val dumped: String = render(finished)
    assert(!dumped.contains("table-with-notes"), dumped)
    assert(!dumped.contains("table-footnotes"), dumped)
    assert(!dumped.contains("_table_"), dumped)
    assert(dumped.contains("""id="_footnote_1""""), dumped)
    assert(dumped.contains("chrome"), dumped)
    val lists: Seq[Xml.Element] = finished.gather(el =>
      Option.when(el.hasClass("footnotes"))(el)
    )
    assert(lists.size == 1, dumped)
    assert(!lists.head.hasClass("table-footnotes"), dumped)
  }

  test("finishFootnotes on collection-index does not letter abstracts") {
    val xml: Xml.Element = XmlParser.parseXml(
      """<div><table class="collection-index"><tr><td>abs<note place="end">when exactly</note></td></tr></table></div>"""
    ).toOption.get
    val finished: Xml.Element = TeiMarkup.finishFootnotes(xml, PageErrorReporter.Silent)
    val dumped: String = render(finished)
    assert(!dumped.contains("table-with-notes"), dumped)
    assert(!dumped.contains("table-footnotes"), dumped)
    assert(dumped.contains("""class="footnotes""""), dumped)
    assert(dumped.contains("""id="_footnote_1""""), dumped)
    assert(dumped.contains("when exactly"), dumped)
    val tableAt: Int = dumped.indexOf("collection-index")
    val footnotesAt: Int = dumped.indexOf("""class="footnotes"""")
    assert(tableAt >= 0 && footnotesAt > tableAt, dumped)
  }

  test("reuse of a document id from inside a note stays arabic, not nested") {
    val xml: Xml.Element = Xml.element(XmlElement.Div).setChildren(Chunk(
      Footnote.link("doc"),
      Footnote.body("doc", Chunk(Xml.text("document note"))),
      Footnote.link("outer"),
      Footnote.body("outer", Chunk(
        Xml.text("see "),
        Footnote.link("doc")
      ))
    ))
    val finished: Xml.Element = Footnote.finish(xml, PageErrorReporter.Silent)
    val dumped: String = render(finished)
    assert(!dumped.contains("nested-footnotes"), dumped)
    assert(!dumped.contains("""data-footnote-scope="nested""""), dumped)
    val docLinks: Seq[Xml.Element] = finished.gather(el =>
      Option.when(el.hasClass("footnote-link") && el.getHref.contains("#_footnote_1"))(el)
    )
    assert(docLinks.size >= 2, dumped)
    assert(docLinks.forall(_.getText == "1"), dumped)
    val topRefs: Seq[Xml.Element] = finished.getChildren.flatMap(_.asElement).filter(_.hasClass("footnote-ref"))
    assert(topRefs.size == 2, dumped)
    assert(dumped.contains("""id="_footnote_1""""), dumped)
    assert(dumped.contains("""id="_footnote_2""""), dumped)
    assert(!dumped.contains("_footnote_1_n_"), dumped)
  }

  test("nested letters and inner list; tooltip has markers and no nested list or nested tip") {
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
    assert(dumped.contains("""id="_footnote_1""""), dumped)
    assert(dumped.contains("""id="_footnote_1_n_a""""), dumped)
    assert(dumped.contains("""id="_footnote_1_n_src_a""""), dumped)
    val srcMarkers: Seq[Xml.Element] = finished.gather(el =>
      Option.when(el.getId.contains("_footnote_1_n_src_a"))(el)
    )
    assert(srcMarkers.size == 1, dumped)
    assert(dumped.contains("""data-footnote-scope="nested""""), dumped)
    assert(dumped.contains("nested-footnotes"), dumped)
    assert(dumped.contains("inner note"), dumped)
    val nestedLists: Seq[Xml.Element] = finished.gather(el =>
      Option.when(el.hasClass("nested-footnotes"))(el)
    )
    assert(nestedLists.size == 1, dumped)
    assert(nestedLists.head.isElement(XmlElement.Span), dumped)
    val outerBodies: Seq[Xml.Element] = finished.gather(el =>
      Option.when(el.getId.contains("_footnote_1"))(el)
    )
    assert(outerBodies.size == 1, dumped)
    assert(outerBodies.head.gather(el =>
      Option.when(el.hasClass("nested-footnotes"))(el)
    ).nonEmpty, dumped)
    val outerTips: Seq[Xml.Element] = finished.gather(el =>
      Option.when(el.hasClass("footnote-tip") && el.getText.contains("outer"))(el)
    )
    assert(outerTips.size == 1, dumped)
    val tipHtml: String = render(outerTips.head)
    assert(tipHtml.contains("""data-footnote-scope="nested""""), tipHtml)
    assert(tipHtml.contains(">a</a>") || tipHtml.contains(">a<"), tipHtml)
    assert(!tipHtml.contains("nested-footnotes"), tipHtml)
    assert(!tipHtml.contains("inner note"), tipHtml)
    val tipSrc: Seq[Xml.Element] = outerTips.head.gather(el =>
      Option.when(el.getId.contains("_footnote_1_n_src_a"))(el)
    )
    assert(tipSrc.isEmpty, tipHtml)
    val nestedTips: Seq[Xml.Element] = outerTips.head.gather(el =>
      Option.when(el.hasClass("footnote-tip"))(el)
    )
    assert(nestedTips.size == 1, tipHtml)
    assert(nestedTips.head eq outerTips.head, tipHtml)
  }

  test("depth greater than one is FootnoteNesting and promotes onto the outermost inner series") {
    val xml: Xml.Element = Xml.element(XmlElement.Div).setChildren(Chunk(
      Footnote.link("c"),
      Footnote.body("c", Chunk(
        Xml.text("C "),
        Footnote.link("b"),
        Footnote.body("b", Chunk(
          Xml.text("B "),
          Footnote.link("a"),
          Footnote.body("a", Chunk(Xml.text("A")))
        ))
      ))
    ))
    val reporter: RecordingReporter = RecordingReporter()
    val finished: Xml.Element = Footnote.finish(xml, reporter)
    val dumped: String = render(finished)
    assert(reporter.errors.exists(_._1 eq PageError.FootnoteNesting), reporter.errors)
    assert(dumped.contains("""id="_footnote_1""""), dumped)
    assert(dumped.contains("""id="_footnote_1_n_a""""), dumped)
    assert(dumped.contains("""id="_footnote_1_n_b""""), dumped)
    assert(dumped.contains("B "), dumped)
    assert(dumped.contains("A"), dumped)
    assert(!dumped.contains("_footnote_1_n_a_n_"), dumped)
    val nestedLists: Seq[Xml.Element] = finished.gather(el =>
      Option.when(el.hasClass("nested-footnotes"))(el)
    )
    assert(nestedLists.size == 1, dumped)
    val nestedHtml: String = render(nestedLists.head)
    assert(nestedHtml.contains("B "), nestedHtml)
    assert(nestedHtml.contains("A"), nestedHtml)
  }

  test("cycle is FootnoteCycle and both bodies still appear in the document list in specified order") {
    val xml: Xml.Element = Xml.element(XmlElement.Div).setChildren(Chunk(
      Footnote.link("d"),
      Footnote.body("d", Chunk(Xml.text("doc"))),
      Footnote.body("a", Chunk(Xml.text("body-a"), Footnote.link("b"))),
      Footnote.body("b", Chunk(Xml.text("body-b"), Footnote.link("a")))
    ))
    val reporter: RecordingReporter = RecordingReporter()
    val finished: Xml.Element = Footnote.finish(xml, reporter)
    val dumped: String = render(finished)
    assert(reporter.errors.exists(_._1 eq PageError.FootnoteCycle), reporter.errors)
    assert(reporter.errors.exists((kind, msg) => (kind eq PageError.FootnoteCycle) && msg.contains("'a'")), reporter.errors)
    assert(reporter.errors.exists((kind, msg) => (kind eq PageError.FootnoteCycle) && msg.contains("'b'")), reporter.errors)
    val lists: Seq[Xml.Element] = finished.gather(el =>
      Option.when(el.hasClass("footnotes") && !el.hasClass("nested-footnotes") && !el.hasClass("table-footnotes"))(el)
    )
    assert(lists.size == 1, dumped)
    val listHtml: String = render(lists.head)
    assert(listHtml.contains("doc"), listHtml)
    assert(listHtml.contains("body-a"), listHtml)
    assert(listHtml.contains("body-b"), listHtml)
    val docAt: Int = listHtml.indexOf("doc")
    val bodyBAt: Int = listHtml.indexOf("body-b")
    val bodyAAt: Int = listHtml.indexOf("body-a")
    assert(docAt >= 0 && bodyBAt > docAt && bodyAAt > bodyBAt, listHtml)
    assert(dumped.contains("""id="_footnote_1""""), dumped)
    assert(dumped.contains("""id="_footnote_2""""), dumped)
    assert(dumped.contains("""id="_footnote_3""""), dumped)
    assert(!dumped.contains("nested-footnotes"), dumped)
  }

  test("nested under a table note uses table-local inner ids") {
    val xml: Xml.Element = Xml.element(XmlElement.Div).setChildren(Chunk(
      cellTable(Chunk(Footnote.link("outer"))),
      Footnote.body("outer", Chunk(
        Xml.text("table outer "),
        Footnote.link("inner"),
        Footnote.body("inner", Chunk(Xml.text("table inner")))
      ))
    ))
    val finished: Xml.Element = finishTables(xml)
    val dumped: String = render(finished)
    assert(dumped.contains("""id="_table_1_fn_a""""), dumped)
    assert(dumped.contains("""id="_table_1_fn_a_n_a""""), dumped)
    assert(dumped.contains("""id="_table_1_fn_a_n_src_a""""), dumped)
    val tableSrc: Seq[Xml.Element] = finished.gather(el =>
      Option.when(el.getId.contains("_table_1_fn_a_n_src_a"))(el)
    )
    assert(tableSrc.size == 1, dumped)
    assert(dumped.contains("""data-footnote-scope="nested""""), dumped)
    assert(dumped.contains("table inner"), dumped)
    val pageLists: Seq[Xml.Element] = finished.gather(el =>
      Option.when(el.hasClass("footnotes") && !el.hasClass("table-footnotes") && !el.hasClass("nested-footnotes"))(el)
    )
    assert(pageLists.isEmpty, dumped)
  }

  test("inner-only FootnoteScopeConflict emits one document body") {
    val xml: Xml.Element = Xml.element(XmlElement.Div).setChildren(Chunk(
      Footnote.link("p1"),
      Footnote.body("p1", Chunk(
        Xml.text("first "),
        Footnote.link("inner")
      )),
      Footnote.link("p2"),
      Footnote.body("p2", Chunk(
        Xml.text("second "),
        Footnote.link("inner")
      )),
      Footnote.body("inner", Chunk(Xml.text("shared inner")))
    ))
    val reporter: RecordingReporter = RecordingReporter()
    val finished: Xml.Element = Footnote.finish(xml, reporter)
    val dumped: String = render(finished)
    assert(reporter.errors.exists(_._1 eq PageError.FootnoteScopeConflict), reporter.errors)
    assert(dumped.contains("shared inner"), dumped)
    assert(!dumped.contains("nested-footnotes"), dumped)
    assert(!dumped.contains("""data-footnote-scope="nested""""), dumped)
    val innerBodies: Seq[Xml.Element] = finished.gather(el =>
      Option.when(el.hasClass("footnote") && el.getId.contains("_footnote_3"))(el)
    )
    assert(innerBodies.size == 1, dumped)
    assert(innerBodies.head.getText.contains("shared inner"), render(innerBodies.head))
    val innerLinks: Seq[Xml.Element] = finished.gather(el =>
      Option.when(el.hasClass("footnote-link") && el.getHref.contains("#_footnote_3"))(el)
    )
    assert(innerLinks.size >= 2, dumped)
    assert(innerLinks.forall(_.getText == "3"), dumped)
  }

  test("appendReferenced omits leftover document bodies when emitLeftovers is false") {
    val treeNote: Footnote = Footnote(
      correlationId = "d",
      number = 1,
      nodes = Chunk(Xml.text("doc "), Footnote.link("a"))
    )
    val leftover: Footnote = Footnote(
      correlationId = "a",
      number = 2,
      nodes = Chunk(Xml.text("leftover-body"))
    )
    val notes: Map[String, Footnote] = Map("d" -> treeNote, "a" -> leftover)
    val xml: Xml.Element = Xml.element(XmlElement.Div).setChildren(Chunk(Footnote.link("d")))
    val withLeftovers: String = render(Footnote.appendReferenced(xml, notes))
    val without: String = render(Footnote.appendReferenced(xml, notes, emitLeftovers = false))
    assert(withLeftovers.contains("leftover-body"), withLeftovers)
    assert(withLeftovers.contains("doc "), withLeftovers)
    assert(without.contains("doc "), without)
    assert(!without.contains("leftover-body"), without)
  }

  test("host tree site is not leftover Document on a tree that omits that site") {
    val xml: Xml.Element = Xml.element(XmlElement.Div).setChildren(Chunk(
      Footnote.link("doc"),
      Footnote.body("doc", Chunk(Xml.text("document note"))),
      Footnote.link("outer"),
      Footnote.body("outer", Chunk(
        Xml.text("see "),
        Footnote.link("doc")
      ))
    ))
    val (notes, stripped) = Footnote.harvest(xml)
    val chunk: Xml.Element = stripped.setChildren(
      stripped.getChildren.filter: node =>
        !node.asElement.exists: el =>
          Footnote.isLink(el) && Footnote.getCorrelationId(el) == "doc"
    )
    val emitted: Map[String, Footnote] = Footnote.numbered(
      chunk,
      notes,
      PageErrorReporter.Silent,
      localTables = true,
      hostTree = Some(stripped),
      hostFootnotes = notes
    )
    assert(emitted.contains("outer"), emitted.keys)
    assert(!emitted.contains("doc"), emitted.keys)
    val reuse: Xml.Element = Xml.element(XmlElement.Span).setChildren(notes("outer").nodes)
      .gather(el => Option.when(Footnote.isLink(el))(el))
      .head
    val resolved: Xml.Element = Footnote.resolveLink(
      reuse,
      notes,
      emitted,
      attachTip = true,
      PageErrorReporter.Silent
    )
    assert(Footnote.isLink(resolved), render(resolved))
    assert(!resolved.isA, render(resolved))
  }

  test("inner-only FootnoteScopeConflict is leftover Document when hostTree is passed") {
    val xml: Xml.Element = Xml.element(XmlElement.Div).setChildren(Chunk(
      Footnote.link("p1"),
      Footnote.body("p1", Chunk(
        Xml.text("first "),
        Footnote.link("inner")
      )),
      Footnote.link("p2"),
      Footnote.body("p2", Chunk(
        Xml.text("second "),
        Footnote.link("inner")
      )),
      Footnote.body("inner", Chunk(Xml.text("shared inner")))
    ))
    val (notes, stripped) = Footnote.harvest(xml)
    val reporter: RecordingReporter = RecordingReporter()
    val emitted: Map[String, Footnote] = Footnote.numbered(
      stripped,
      notes,
      reporter,
      localTables = true,
      hostTree = Some(stripped),
      hostFootnotes = notes
    )
    assert(reporter.errors.exists(_._1 eq PageError.FootnoteScopeConflict), reporter.errors)
    assert(emitted.contains("inner"), emitted.keys)
    assert(emitted("inner").scope == FootnoteScope.Document)
  }

  test("emitLeftovers false omits leftover Document from emitted so inner stub stays IR") {
    val xml: Xml.Element = Xml.element(XmlElement.Div).setChildren(Chunk(
      Footnote.link("p1"),
      Footnote.body("p1", Chunk(
        Xml.text("first "),
        Footnote.link("inner")
      )),
      Footnote.link("p2"),
      Footnote.body("p2", Chunk(
        Xml.text("second "),
        Footnote.link("inner")
      )),
      Footnote.body("inner", Chunk(Xml.text("shared inner")))
    ))
    val (notes, stripped) = Footnote.harvest(xml)
    val chunk: Xml.Element = stripped.setChildren(
      stripped.getChildren.filter: node =>
        !node.asElement.exists: el =>
          Footnote.isLink(el) && Footnote.getCorrelationId(el) == "p2"
    )
    val emitted: Map[String, Footnote] = Footnote.numbered(
      chunk,
      notes,
      PageErrorReporter.Silent,
      localTables = true,
      hostTree = Some(stripped),
      hostFootnotes = notes,
      emitLeftovers = false
    )
    assert(emitted.contains("p1"), emitted.keys)
    assert(!emitted.contains("inner"), emitted.keys)
    val innerStub: Xml.Element = Xml.element(XmlElement.Span).setChildren(notes("p1").nodes)
      .gather(el => Option.when(Footnote.isLink(el))(el))
      .head
    val resolved: Xml.Element = Footnote.resolveLink(
      innerStub,
      notes,
      emitted,
      attachTip = true,
      PageErrorReporter.Silent
    )
    assert(Footnote.isLink(resolved), render(resolved))
    assert(!resolved.isA, render(resolved))
  }
