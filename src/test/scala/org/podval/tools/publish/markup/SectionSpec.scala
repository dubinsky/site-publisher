package org.podval.tools.publish.markup

import org.asciidoctor.Asciidoctor
import org.podval.tools.publish.site.{PageError, PageErrorReporter}
import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.{Html, HtmlXmlDialect, Xml, XmlAttribute, XmlParser}
import org.scalatest.funsuite.AnyFunSuite
import zio.blocks.chunk.Chunk
import java.io.File

final class SectionSpec extends AnyFunSuite:
  private def render(element: Xml.Element): String = HtmlXmlDialect.render(element)

  private def normalizeTree(xml: Xml.Element): Xml.Element =
    val ids: IdGenerator = IdGenerator("_id")
    xml.transform(Section.normalize(_, ids))

  test("normalize copies xml:id to id") {
    val xml: Xml.Element = XmlParser.parseXml(
      """<div xml:id="colophon"><p>body</p></div>"""
    ).toOption.get
    assert(xml.get(XmlAttribute.XmlId).contains("colophon"))
    val copied: Xml.Element = xml.copyXmlId
    assert(copied.getId.contains("colophon"))
    assert(copied.get(XmlAttribute.XmlId).contains("colophon"))
  }

  test("copyXmlId does not overwrite an existing id") {
    val xml: Xml.Element = Xml
      .element("div")
      .setId("keep")
      .set(XmlAttribute.XmlId, "other")
    assert(xml.copyXmlId.getId.contains("keep"))
  }

  test("normalize adds permalinks from the section id for HTML headings") {
    val xml: Xml.Element = XmlParser.parseXml(
      """<div><h2 id="colophon">Colophon</h2><p>body</p></div>"""
    ).toOption.get
    val nested: Xml.Element = xml.setChildren(HtmlMarkup.nestSections(xml.getChildren))
    val normalized: Xml.Element = normalizeTree(nested)
    val rendered: String = render(normalized)
    assert(rendered.contains("""class="section""""))
    assert(rendered.contains("""id="colophon""""))
    assert(rendered.contains("""class="anchor""""))
    assert(rendered.contains("""class="link""""))
    assert(rendered.contains("""class="heading""""))
    assert(rendered.contains("""href="#colophon""""))
    assert(!rendered.contains("""<h2 id="colophon""""))
  }

  test("normalize uses the section id when the heading has none") {
    val xml: Xml.Element = XmlParser.parseXml(
      """<div><h2>Notes</h2><p>body</p></div>"""
    ).toOption.get
    val nested: Xml.Element = xml.setChildren(HtmlMarkup.nestSections(xml.getChildren))
    val normalized: Xml.Element = normalizeTree(nested)
    val rendered: String = render(normalized)
    assert(rendered.contains("""id="Notes""""))
    assert(rendered.contains("""href="#Notes""""))
  }

  test("heading skips leading non-heading children") {
    val xml: Xml.Element = XmlParser.parseXml(
      """<div xml:id="meth"><pb n="1"/><fw type="pageNum">3</fw><head>Methodology</head><p>body</p></div>"""
    ).toOption.get
    val converted: Xml.Element = xml.transform(element =>
      if element.getName == "head" then element.rename("tei-head") else element
    )
    val marked: Xml.Element = TeiMarkup.markHeadedDivs(converted)
    val header: Xml.Element = Section.heading(marked).get
    assert(header.getName == "tei-head")
    assert(header.getText == "Methodology")
    assert(Section.is(marked))
  }

  test("TEI marks only headed divs; grouping divs stay unmarked") {
    val xml: Xml.Element = XmlParser.parseXml(
      """<div>
        |  <div><p>wrapper</p>
        |    <div xml:id="meth"><head>Methodology</head><p>body</p></div>
        |  </div>
        |</div>"""
    ).toOption.get
    val converted: Xml.Element = xml.transform(element =>
      if element.getName == "head" then element.rename("tei-head") else element
    )
    val marked: Xml.Element = TeiMarkup.markHeadedDivs(converted)
    val outer: Xml.Element = marked.getChildren.flatMap(_.asElement).head
    val inner: Xml.Element = outer.getChildren.flatMap(_.asElement).find(_.getName == "div").get
    assert(!Section.is(marked))
    assert(!Section.is(outer))
    assert(Section.is(inner))
  }

  test("Toc walks through non-section wrappers to headed sections") {
    val inner: Xml.Element = Section
      .mark(Xml.element("div"))
      .setId("meth")
      .setChildren(Chunk(
        Section.markHeading(Xml.element("tei-head").setChildren(Chunk(Xml.text("Methodology")))),
        Xml.element("p").setChildren(Chunk(Xml.text("body")))
      ))
    val grouping: Xml.Element = Xml.element("div").setChildren(Chunk(
      Xml.element("p").setChildren(Chunk(Xml.text("wrapper"))),
      inner
    ))
    val root: Xml.Element = Xml.element("TEI").setChildren(Chunk(grouping))
    val toc: Toc = Toc(root, PageErrorReporter.Silent)
    assert(toc.flatten.map(_.id) == Seq("meth"))
    assert(toc.flatten.head.title == "Methodology")
  }

  test("normalize adds permalinks for TEI tei-head using the div id") {
    val section: Xml.Element = Section
      .mark(Xml.element("div"))
      .set(XmlAttribute.XmlId, "methodology")
      .setChildren(Chunk(
        Section.markHeading(Xml.element("tei-head").setChildren(Chunk(Xml.text("Methodology")))),
        Xml.element("p").setChildren(Chunk(Xml.text("body")))
      ))
    val normalized: Xml.Element = Section.normalize(section, IdGenerator("_id"))
    val rendered: String = render(normalized)
    assert(normalized.getId.contains("methodology"))
    assert(rendered.contains("""id="methodology""""))
    assert(rendered.contains("""class="heading""""))
    assert(rendered.contains("""href="#methodology""""))
    assert(rendered.contains("Methodology"))
    assert(!rendered.contains("<tei-head id="))
  }

  private lazy val asciidoctor: Asciidoctor = Asciidoctor.Factory.create()

  private def parse(xml: String): Xml.Element = XmlParser.parseXml(xml).toOption.get

  private def ir(processed: Xml.Element): Xml.Element =
    normalizeTree(processed)

  private def tocTitles(xml: Xml.Element): Seq[(String, String)] =
    Toc(xml, PageErrorReporter.Silent).flatten.map(section => (section.id, section.title))

  private def assertPermalink(rendered: String, id: String, title: String): Unit =
    assert(rendered.contains(s"""id="$id""""), rendered)
    assert(rendered.contains(s"""href="#$id""""), rendered)
    assert("""class="[^"]*heading""".r.findFirstIn(rendered).isDefined, rendered)
    assert(rendered.contains("""class="anchor""""), rendered)
    assert(rendered.contains("""class="link""""), rendered)
    assert(rendered.contains(title), rendered)
    assert(!rendered.contains(s"""<h2 id="$id""""), rendered)
    assert(!rendered.contains(s"""<h3 id="$id""""), rendered)
    assert(!rendered.contains(s"""<tei-head id="$id""""), rendered)

  test("HTML nested headings become nested sections with permalinks") {
    val xml: Xml.Element = parse(
      """<div><h1>Book</h1><p>intro</p><h2 id="chapter">Chapter</h2><p>chap</p><h3 id="notes">Notes</h3><p>sec</p></div>"""
    )
    val processed: Xml.Element = HtmlMarkup.process(xml, PageErrorReporter.Silent)._1
    val normalized: Xml.Element = ir(processed)
    val rendered: String = render(normalized)
    assertPermalink(rendered, "chapter", "Chapter")
    assertPermalink(rendered, "notes", "Notes")
    assert(tocTitles(normalized) == Seq("chapter" -> "Chapter", "notes" -> "Notes"))
    val top: Section = Toc(normalized, PageErrorReporter.Silent).sections.head
    assert(top.id == "chapter")
    assert(top.sections.map(_.id) == Seq("notes"))
  }

  test("Markdown nested headings become nested sections with permalinks") {
    val xml: Xml.Element = parse(MarkdownMarkup.xmlContent(
      """# Book
        |intro
        |## Chapter
        |chap
        |### Notes
        |sec
        |""".stripMargin,
      File("t.md")
    ))
    val processed: Xml.Element = HtmlMarkup.process(MarkdownMarkup.convert(xml), PageErrorReporter.Silent)._1
    val normalized: Xml.Element = ir(processed)
    val rendered: String = render(normalized)
    assertPermalink(rendered, "Chapter", "Chapter")
    assertPermalink(rendered, "Notes", "Notes")
    assert(tocTitles(normalized) == Seq("Chapter" -> "Chapter", "Notes" -> "Notes"))
  }

  test("AsciiDoc nested headings become nested sections with permalinks") {
    val xml: Xml.Element = parse(AsciiDocMarkup.convert(
      """= Book
        |
        |intro
        |
        |== Chapter
        |
        |chap
        |
        |=== Notes
        |
        |sec
        |""".stripMargin,
      File("t.adoc").getAbsoluteFile,
      asciidoctor
    ))
    val processed: Xml.Element = HtmlMarkup.process(AsciiDocMarkup.cleanup(xml), PageErrorReporter.Silent)._1
    val normalized: Xml.Element = ir(processed)
    val rendered: String = render(normalized)
    assertPermalink(rendered, "Chapter", "Chapter")
    assertPermalink(rendered, "Notes", "Notes")
    assert(tocTitles(normalized) == Seq("Chapter" -> "Chapter", "Notes" -> "Notes"))
  }

  test("TEI headed divs, grouping wrappers, leading pb, nested sections") {
    val xml: Xml.Element = parse(
      """<TEI><text><body>
        |<div><p>bundle</p>
        |  <div xml:id="meth"><pb n="1"/><head>Methodology</head><p>body</p>
        |    <div xml:id="notes"><head>Notes</head><p>sec</p></div>
        |  </div>
        |</div>
        |</body></text></TEI>"""
    )
    val processed: Xml.Element = TeiMarkup.process(xml, PageErrorReporter.Silent)._1
    val normalized: Xml.Element = ir(processed)
    val rendered: String = render(normalized)
    assertPermalink(rendered, "meth", "Methodology")
    assertPermalink(rendered, "notes", "Notes")
    assert(tocTitles(normalized) == Seq("meth" -> "Methodology", "notes" -> "Notes"))
    val top: Section = Toc(normalized, PageErrorReporter.Silent).sections.head
    assert(top.id == "meth")
    assert(top.sections.map(_.id) == Seq("notes"))
  }

  test("DocBook nested sections become headed div.section with db-title") {
    val xml: Xml.Element = parse(
      """<article>
        |  <section xml:id="meth"><title>Methodology</title><para>body</para>
        |    <section xml:id="notes"><title>Notes</title><para>sec</para></section>
        |  </section>
        |</article>"""
    )
    val processed: Xml.Element = DocBookMarkup.process(xml, PageErrorReporter.Silent)._1
    val normalized: Xml.Element = ir(processed)
    val rendered: String = render(normalized)
    assertPermalink(rendered, "meth", "Methodology")
    assertPermalink(rendered, "notes", "Notes")
    assert(tocTitles(normalized) == Seq("meth" -> "Methodology", "notes" -> "Notes"))
    val top: Section = Toc(normalized, PageErrorReporter.Silent).sections.head
    assert(top.id == "meth")
    assert(top.sections.map(_.id) == Seq("notes"))
    assert(rendered.contains("<db-title"), rendered)
  }

  private def tocHtml(toc: Toc, current: Option[String], tocDepth: Int = 2): String =
    HtmlXmlDialect.render(toc.html(current, tocDepth, chunkDepth = Some(2)))

  test("chunked TOC expands current path and leaves other branches as titles") {
    val a1: Section = Section("a1", "A1", Seq.empty)
    val a2: Section = Section("a2", "A2", Seq.empty)
    val b1: Section = Section("b1", "B1", Seq.empty)
    val b2: Section = Section("b2", "B2", Seq.empty)
    val a: Section = Section("a", "A", Seq(a1, a2))
    val b: Section = Section("b", "B", Seq(b1, b2))
    val toc: Toc = new Toc(Seq(a, b))
    val full: String = tocHtml(toc, None)
    assert(full.contains("A1"))
    assert(full.contains("B1"))
    assert(!full.contains("toc-current"))
    val chunk: String = tocHtml(toc, Some("a1"))
    assert(chunk.contains("A1"))
    assert(chunk.contains("A2"))
    assert(chunk.contains("""class="toc-section toc-current""""))
    assert(chunk.contains("""class="toc-section toc-ancestor""""))
    assert(chunk.contains(">B<") || chunk.contains(">B</a>"))
    assert(!chunk.contains("B1"), chunk)
    assert(!chunk.contains("B2"), chunk)
  }

  test("marked section without a heading is a defect") {
    val section: Xml.Element = Section
      .mark(Xml.element("div"))
      .setId("foo")
      .setChildren(Chunk(Xml.element("p").setChildren(Chunk(Xml.text("body")))))
    val thrown: IllegalStateException = intercept[IllegalStateException]:
      Toc(section, PageErrorReporter.Silent)
    assert(thrown.getMessage.contains("foo"), thrown.getMessage)
  }

  test("empty section heading is reported") {
    var reported: Option[(PageError.Kind, String)] = None
    val reporter: PageErrorReporter = new PageErrorReporter:
      override def error(
        kind: PageError.Kind,
        message: String,
        cause: Option[Throwable] = None
      ): Unit = reported = Some((kind, message))
    val section: Xml.Element = Section
      .mark(Xml.element("div"))
      .setId("foo")
      .setChildren(Chunk(
        Section.markHeading(Xml.element("h2")),
        Xml.element("p").setChildren(Chunk(Xml.text("body")))
      ))
    val toc: Toc = Toc(section, reporter)
    assert(reported.contains((PageError.NoTitle, "No title on section foo")))
    assert(toc.flatten.head.title == "foo")
  }

  test("resolveSection finds a nested section by title or id") {
    val notes: Section = Section("notes", "Notes", Seq.empty)
    val chapter: Section = Section("chapter", "Chapter", Seq(notes))
    val toc: Toc = new Toc(Seq(chapter, Section("other", "Other", Seq.empty)))
    assert(toc.resolveSection(Seq("Notes")).map(_.id).contains("notes"))
    assert(toc.resolveSection(Seq("notes")).map(_.id).contains("notes"))
    assert(toc.resolveSection(Seq("Chapter", "Notes")).map(_.id).contains("notes"))
    val branched: Toc = new Toc(Seq(
      Section("a", "A", Seq(Section("a1", "A1", Seq.empty))),
      Section("b", "B", Seq(Section("b1", "B1", Seq.empty)))
    ))
    assert(branched.resolveSection(Seq("B1")).map(_.id).contains("b1"))
    assert(branched.resolveSection(Seq("A1")).map(_.id).contains("a1"))
  }

  test("resolveSection does not match the first section for an unknown name") {
    val toc: Toc = new Toc(Seq(Section("a", "A", Seq(Section("a1", "A1", Seq.empty)))))
    assert(toc.resolveSection(Seq("nope")).isEmpty)
    assert(toc.resolveSection(Seq("A", "nope")).isEmpty)
  }

  test("unchunked TOC still lists every branch to tocDepth") {
    val a1: Section = Section("a1", "A1", Seq.empty)
    val b1: Section = Section("b1", "B1", Seq.empty)
    val toc: Toc = new Toc(Seq(Section("a", "A", Seq(a1)), Section("b", "B", Seq(b1))))
    val rendered: String = HtmlXmlDialect.render(toc.html(None, tocDepth = 2, chunkDepth = None))
    assert(rendered.contains("A1"))
    assert(rendered.contains("B1"))
  }
