package org.podval.tools.publish.markup

import de.undercouch.citeproc.bibtex.{BibTeXConverter, BibTeXItemDataProvider}
import org.asciidoctor.Asciidoctor
import org.podval.tools.publish.page.FrontMatter
import org.podval.tools.publish.site.PageErrorReporter
import org.podval.xml.{HtmlXmlWriterConfig, Xml, XmlParser}
import org.scalatest.funsuite.AnyFunSuite
import zio.blocks.chunk.Chunk
import java.io.{ByteArrayInputStream, File}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files as NioFiles, Path as NioPath}

final class CitationSpec extends AnyFunSuite:
  private lazy val asciidoctor: Asciidoctor =
    val result: Asciidoctor = Asciidoctor.Factory.create()
    AsciiDocCiteExtension.register(result)
    result

  private val knuthLamportBib: String =
    """@book{knuth79,
      |  author = {Knuth, Donald E.},
      |  title = {TeX and Metafont},
      |  year = {1979},
      |  publisher = {Digital Press}
      |}
      |@article{lamport94,
      |  author = {Lamport, Leslie},
      |  title = {LaTeX},
      |  year = {1994},
      |  journal = {Somewhere}
      |}
      |""".stripMargin

  private def bibliography(style: String, bib: String = knuthLamportBib): Bibliography =
    val provider: BibTeXItemDataProvider = BibTeXItemDataProvider()
    provider.addDatabase(BibTeXConverter().loadDatabase(
      ByteArrayInputStream(bib.getBytes(StandardCharsets.UTF_8))
    ))
    Bibliography(Some(provider), style, "en-US")

  private def render(element: Xml.Element): String = HtmlXmlWriterConfig.render(element)

  private def parse(xml: String): Xml.Element = XmlParser.parseXml(xml).toOption.get

  private def cites(xml: Xml.Element): Seq[Xml.Element] =
    Citation.gather(xml).toSeq

  private def citeItems(xml: Xml.Element): Seq[(Citation.Mode, Citation.Item)] =
    cites(xml).flatMap(c => Citation.itemsOf(c).map(Citation.modeOf(c) -> _))

  private def wrap(nodes: Xml.Node*): Xml.Element =
    Xml.element("div").setChildren(Chunk.from(nodes))

  private def withTempDir(body: File => Unit): Unit =
    val path: NioPath = NioFiles.createTempDirectory("site-publisher-bib")
    try body(path.toFile)
    finally
      NioFiles.walk(path).sorted(java.util.Comparator.reverseOrder()).forEach(NioFiles.delete(_))

  test("CiteMacro.parseTarget splits keys and locators") {
    val one: Seq[Citation.Item] = CiteMacro.parseTarget("knuth79")
    assert(one.map(_.key) == Seq("knuth79"))
    assert(one.head.locator.isEmpty)
    val located: Seq[Citation.Item] = CiteMacro.parseTarget("knuth79, p. 12")
    assert(located.map(_.key) == Seq("knuth79"))
    assert(located.head.locator.contains("p. 12"))
    val two: Seq[Citation.Item] = CiteMacro.parseTarget("knuth79, lamport94")
    assert(two.map(_.key) == Seq("knuth79", "lamport94"))
  }

  test("CiteMacro.raw joins target and all positional attributes") {
    val attrs = new java.util.HashMap[String, Object]()
    attrs.put("1", "knuth79")
    attrs.put("2", "lamport94")
    assert(CiteMacro.raw("", attrs) == "knuth79, lamport94")
    attrs.clear()
    attrs.put("1", "p. 12")
    assert(CiteMacro.raw("knuth79", attrs) == "knuth79, p. 12")
    attrs.clear()
    attrs.put("1", "knuth79")
    attrs.put("2", "p. 12")
    assert(CiteMacro.raw("", attrs) == "knuth79, p. 12")
  }

  test("AsciiDoc cite:[key] and bibliography::[] become IR stubs") {
    val xml: Xml.Element = parse(AsciiDocMarkup.convert(
      """See cite:[knuth79] and citenp:[lamport94] and cite:knuth79[p. 12].
        |
        |cite:[knuth79, p. 12] and cite:[knuth79, lamport94] and citenp:lamport94[].
        |
        |bibliography::[]
        |""".stripMargin,
      File("t.adoc").getAbsoluteFile,
      asciidoctor
    ))
    val cleaned: Xml.Element = AsciiDocMarkup.cleanup(xml)
    val items: Seq[(Citation.Mode, Citation.Item)] = citeItems(cleaned)
    val dumped: String = render(cleaned)
    assert(items.exists((mode, item) => mode == Citation.Mode.Parenthetical && item.key == "knuth79" && item.locator.isEmpty), dumped)
    assert(items.exists((mode, item) => mode == Citation.Mode.Narrative && item.key == "lamport94"), dumped)
    assert(items.exists((mode, item) => mode == Citation.Mode.Parenthetical && item.key == "knuth79" && item.locator.contains("p. 12")), dumped)
    assert(items.exists((_, item) => item.key == "knuth79") && items.exists((_, item) => item.key == "lamport94"), dumped)
    val multi: Option[Xml.Element] = cites(cleaned).find(c => Citation.itemsOf(c).map(_.key) == Seq("knuth79", "lamport94"))
    assert(multi.isDefined, dumped)
    assert(dumped.contains("""class="bibliography""""), dumped)
  }

  test("AsciiDoc cleanup keeps bibliography div") {
    val xml: Xml.Element = parse("""<div><div class="bibliography"></div></div>""")
    val cleaned: String = render(AsciiDocMarkup.cleanup(xml))
    assert(cleaned.contains("""class="bibliography""""))
  }

  test("AsciiDoc [bibliography] [[[id]]] and <<id>> become native items, not citeproc stubs") {
    val xml: Xml.Element = parse(AsciiDocMarkup.convert(
      """See <<knuth-book>> and <<lamport94>> and cite:[knuth79].
        |
        |[bibliography]
        |* [[[knuth-book]]] Knuth, Donald E. _The Art of Computer Programming_. 1968.
        |* [[[lamport94,Lamport 94]]] Lamport. _LaTeX_. 1994.
        |
        |bibliography::[]
        |""".stripMargin,
      File("t.adoc").getAbsoluteFile,
      asciidoctor
    ))
    val cleaned: Xml.Element = AsciiDocMarkup.cleanup(xml)
    val dumped: String = render(cleaned)
    val items: Seq[Xml.Element] =
      cleaned.gather(el => Option.when(BibliographyItem.isItem(el))(el)).toSeq
    assert(items.map(_.getId).toSet == Set(Some("knuth-book"), Some("lamport94")), dumped)
    assert(cleaned.gather(el => Option.when(BibliographyItem.isList(el))(el)).nonEmpty, dumped)
    assert(dumped.contains("""href="#knuth-book""""), dumped)
    assert(dumped.contains("""href="#lamport94""""), dumped)
    assert(dumped.contains("Lamport 94"), dumped)
    assert(!dumped.contains("""<a id="knuth-book""""), dumped)
    val stubs: Seq[(Citation.Mode, Citation.Item)] = citeItems(cleaned)
    assert(stubs.exists((_, item) => item.key == "knuth79"), dumped)
    assert(!stubs.exists((_, item) => item.key == "knuth-book"), dumped)
    val placeholders: Seq[Xml.Element] =
      cleaned.gather(el => Option.when(Citation.isPlaceholder(el))(el)).toSeq
    assert(placeholders.size == 1, dumped)
    val defs: Map[String, Xml.Nodes] = BibliographyItem.definitions(cleaned)
    assert(defs.keySet == Set("knuth-book", "lamport94"), dumped)
    assert(Xml.toString(defs("knuth-book")).contains("Knuth"), dumped)
  }

  test("resolve fills citeproc placeholder and leaves a native bibliography list") {
    val bib: Bibliography = bibliography("apa")
    val stub: Xml.Element = Citation.cite(Citation.Mode.Parenthetical, Seq(Citation.Item("knuth79")))
    val nativeItem: Xml.Element =
      Xml.element("li").add(BibliographyItem.ItemClass).setId("knuth-book").setText("Knuth book")
    val nativeList: Xml.Element =
      Xml.element("ul").add(Citation.ListClass).setChildren(Chunk(nativeItem))
    val xml: Xml.Element = wrap(
      Xml.element("p").setChildren(Chunk(stub)),
      nativeList,
      Citation.listPlaceholder
    )
    val (resolved, labels) = bib.resolve(xml)
    val dumped: String = render(resolved)
    assert(labels.isEmpty, labels)
    val native: Seq[Xml.Element] =
      resolved.gather(el => Option.when(BibliographyItem.isList(el))(el)).toSeq
    assert(native.size == 1, dumped)
    assert(native.head.getChildren.flatMap(_.asElement).exists(_.getId.contains("knuth-book")), dumped)
    val generated: Seq[Xml.Element] =
      resolved.gather(el => Option.when(Citation.isList(el))(el)).toSeq
    assert(generated.size == 1, dumped)
    assert(dumped.contains(s"""id="${Citation.entryId("knuth79")}""""), dumped)
    assert(dumped.contains("""id="knuth-book""""), dumped)
    assert(dumped.contains(s"""href="${Citation.entryHref("knuth79")}""""), dumped)
  }

  test("Markdown [@key], locator, narrative @key; email left alone") {
    val xml: Xml.Element = parse(MarkdownMarkup.xmlContent(
      """See [@knuth79] and [@knuth79, p. 12] and @lamport94.
        |
        |Email me@host.com please.
        |
        |:::bibliography
        |""".stripMargin,
      File("t.md")
    ))
    val converted: Xml.Element = MarkdownCite.convertElement(xml)
    val items: Seq[(Citation.Mode, Citation.Item)] = citeItems(converted)
    val dumped: String = render(converted)
    assert(items.exists((_, item) => item.key == "knuth79" && item.locator.isEmpty), dumped)
    assert(items.exists((_, item) => item.key == "knuth79" && item.locator.contains("p. 12")), dumped)
    assert(items.exists((mode, item) => mode == Citation.Mode.Narrative && item.key == "lamport94"), dumped)
    assert(dumped.contains("me@host.com"), dumped)
    assert(dumped.contains("""class="bibliography""""), dumped)
  }

  test("Markdown [-@key] and [@key1; @key2]") {
    val xml: Xml.Element = parse(MarkdownMarkup.xmlContent(
      "See [-@knuth79] and [@knuth79; @lamport94].",
      File("t.md")
    ))
    val converted: Xml.Element = MarkdownCite.convertElement(xml)
    val items: Seq[(Citation.Mode, Citation.Item)] = citeItems(converted)
    val dumped: String = render(converted)
    assert(items.exists((mode, item) => mode == Citation.Mode.SuppressAuthor && item.key == "knuth79"), dumped)
    val multi: Option[Xml.Element] =
      cites(converted).find(c => Citation.itemsOf(c).map(_.key) == Seq("knuth79", "lamport94"))
    assert(multi.isDefined, dumped)
    assert(Citation.modeOf(multi.get) == Citation.Mode.Parenthetical)
  }

  test("DocBook citation and empty bibliography become citeproc stubs") {
    val xml: Xml.Element = DocBookMarkup.process(
      parse(
        """<article>
          |<para>See <citation>knuth79</citation> and <citation>knuth79, p. 12</citation>
          |and <biblioref linkend="lamport94"/>.</para>
          |<bibliography/>
          |</article>""".stripMargin
      ),
      PageErrorReporter.Silent
    )._1
    val items: Seq[(Citation.Mode, Citation.Item)] = citeItems(xml)
    val dumped: String = render(xml)
    assert(items.exists((_, item) => item.key == "knuth79" && item.locator.isEmpty), dumped)
    assert(items.exists((_, item) => item.key == "knuth79" && item.locator.contains("p. 12")), dumped)
    assert(items.exists((_, item) => item.key == "lamport94"), dumped)
    assert(xml.gather(el => Option.when(Citation.isPlaceholder(el))(el)).size == 1, dumped)
    assert(!dumped.contains("db-class"), dumped)
  }

  test("DocBook bibliography entries are native items, not citeproc stubs") {
    val xml: Xml.Element = DocBookMarkup.process(
      parse(
        """<article>
          |<para>See <link linkend="knuth79">Knuth 1979</link>
          |and <biblioref linkend="lamport94"/>.</para>
          |<bibliography>
          |  <biblioentry xml:id="knuth79">Knuth, Donald E.</biblioentry>
          |  <bibliomixed xml:id="lamport94">Lamport, Leslie. LaTeX. 1994.</bibliomixed>
          |</bibliography>
          |</article>""".stripMargin
      ),
      PageErrorReporter.Silent
    )._1
    val dumped: String = render(xml)
    val items: Seq[Xml.Element] =
      xml.gather(el => Option.when(BibliographyItem.isItem(el))(el)).toSeq
    assert(items.map(_.getId).toSet == Set(Some("knuth79"), Some("lamport94")), dumped)
    assert(xml.gather(el => Option.when(BibliographyItem.isList(el))(el)).nonEmpty, dumped)
    assert(dumped.contains("""href="#knuth79""""), dumped)
    assert(citeItems(xml).isEmpty, dumped)
  }

  test("MarkdownMarkup.process converts Pandoc cites") {
    val xml: Xml.Element = parse(MarkdownMarkup.xmlContent(
      "See [@knuth79] and [-@lamport94].",
      File("t.md")
    ))
    val processed: Xml.Element = MarkdownMarkup.process(xml, PageErrorReporter.Silent)._1
    val items: Seq[(Citation.Mode, Citation.Item)] = citeItems(processed)
    val dumped: String = render(processed)
    assert(items.exists((mode, item) => mode == Citation.Mode.Parenthetical && item.key == "knuth79"), dumped)
    assert(items.exists((mode, item) => mode == Citation.Mode.SuppressAuthor && item.key == "lamport94"), dumped)
  }

  test("resolve APA in-text and cited-only list; unknown key is unresolved") {
    val bib: Bibliography = bibliography("apa")
    val stub: Xml.Element = Citation.cite(Citation.Mode.Parenthetical, Seq(Citation.Item("knuth79")))
    val unknown: Xml.Element = Citation.cite(Citation.Mode.Parenthetical, Seq(Citation.Item("missing")))
    val (replacements, list) = bib.format(Seq(stub, unknown))
    val knownHtml: String = render(replacements(stub))
    val unknownHtml: String = render(replacements(unknown))
    assert(knownHtml.toLowerCase.contains("knuth"), knownHtml)
    assert(knownHtml.contains(s"""href="${Citation.entryHref("knuth79")}""""), knownHtml)
    assert(unknownHtml.contains("unresolved-citation"), unknownHtml)
    assert(!unknownHtml.contains("href="), unknownHtml)
    val listHtml: String = render(list.get)
    assert(listHtml.toLowerCase.contains("knuth"), listHtml)
    assert(!listHtml.toLowerCase.contains("lamport"), listHtml)
    assert(listHtml.contains(s"""id="${Citation.entryId("knuth79")}""""), listHtml)
  }

  test("IEEE numbers citations in document order") {
    val bib: Bibliography = bibliography("ieee")
    val first: Xml.Element = Citation.cite(Citation.Mode.Parenthetical, Seq(Citation.Item("knuth79")))
    val second: Xml.Element = Citation.cite(Citation.Mode.Parenthetical, Seq(Citation.Item("lamport94")))
    val (replacements, _) = bib.format(Seq(first, second))
    assert(render(replacements(first)).contains("[1]"), render(replacements(first)))
    assert(render(replacements(first)).contains(s"""href="${Citation.entryHref("knuth79")}""""), render(replacements(first)))
    assert(render(replacements(second)).contains("[2]"), render(replacements(second)))
    assert(render(replacements(second)).contains(s"""href="${Citation.entryHref("lamport94")}""""), render(replacements(second)))
  }

  test("APA locator, narrative, and suppress-author") {
    val bib: Bibliography = bibliography("apa")
    val located: Xml.Element =
      Citation.cite(Citation.Mode.Parenthetical, Seq(Citation.Item("knuth79", Some("p. 12"))))
    val narrative: Xml.Element =
      Citation.cite(Citation.Mode.Narrative, Seq(Citation.Item("knuth79")))
    val suppress: Xml.Element =
      Citation.cite(Citation.Mode.SuppressAuthor, Seq(Citation.Item("knuth79")))
    val parenthetical: Xml.Element =
      Citation.cite(Citation.Mode.Parenthetical, Seq(Citation.Item("knuth79")))
    val (replacements, _) = bib.format(Seq(located, narrative, suppress, parenthetical))
    val locatedHtml: String = render(replacements(located))
    val narrativeHtml: String = render(replacements(narrative))
    val suppressHtml: String = render(replacements(suppress))
    val parentheticalHtml: String = render(replacements(parenthetical))
    assert(locatedHtml.contains("12"), locatedHtml)
    // citeproc-java 3's native renderer does not honor author-only / suppress-author;
    // those modes still format a normal in-text citation.
    assert(narrativeHtml.toLowerCase.contains("knuth"), narrativeHtml)
    assert(suppressHtml.contains("1979"), suppressHtml)
    assert(parentheticalHtml.toLowerCase.contains("knuth"), parentheticalHtml)
  }

  test("multi-key cite links to the first key's bibliography entry") {
    val bib: Bibliography = bibliography("apa")
    val stub: Xml.Element = Citation.cite(
      Citation.Mode.Parenthetical,
      Seq(Citation.Item("knuth79"), Citation.Item("lamport94"))
    )
    val (replacements, list) = bib.format(Seq(stub))
    val citeHtml: String = render(replacements(stub))
    assert(citeHtml.contains(s"""href="${Citation.entryHref("knuth79")}""""), citeHtml)
    assert(!citeHtml.contains(Citation.entryHref("lamport94")), citeHtml)
    val listHtml: String = render(list.get)
    assert(listHtml.contains(s"""id="${Citation.entryId("knuth79")}""""), listHtml)
    assert(listHtml.contains(s"""id="${Citation.entryId("lamport94")}""""), listHtml)
  }

  test("no bibliography file leaves stubs unresolved and emits no list") {
    val bib: Bibliography = Bibliography(None, "apa", "en-US")
    val stub: Xml.Element = Citation.cite(Citation.Mode.Parenthetical, Seq(Citation.Item("knuth79")))
    val (replacements, list) = bib.format(Seq(stub))
    assert(list.isEmpty)
    assert(render(replacements(stub)).contains("unresolved-citation"), render(replacements(stub)))
  }

  test("resolve fills bibliography placeholder; unknown keys are reported") {
    val bib: Bibliography = bibliography("apa")
    val stub: Xml.Element = Citation.cite(Citation.Mode.Parenthetical, Seq(Citation.Item("knuth79")))
    val unknown: Xml.Element = Citation.cite(Citation.Mode.Parenthetical, Seq(Citation.Item("missing")))
    val xml: Xml.Element = wrap(
      Xml.element("p").setChildren(Chunk(stub)),
      Xml.element("p").setChildren(Chunk(unknown)),
      Citation.listPlaceholder
    )
    val (resolved, labels) = bib.resolve(xml)
    val dumped: String = render(resolved)
    assert(labels == Seq("missing"), labels)
    assert(dumped.toLowerCase.contains("knuth"), dumped)
    assert(!dumped.toLowerCase.contains("lamport"), dumped)
    assert(dumped.contains("unresolved-citation"), dumped)
    assert(dumped.contains("csl-bib-body") || dumped.contains("csl-entry"), dumped)
    val lists: Seq[Xml.Element] =
      resolved.getChildren.flatMap(_.asElement).filter(Citation.isList).toSeq
    assert(lists.size == 1, dumped)
  }

  test("resolve appends bibliography when the page has no placeholder") {
    val bib: Bibliography = bibliography("apa")
    val stub: Xml.Element = Citation.cite(Citation.Mode.Parenthetical, Seq(Citation.Item("knuth79")))
    val xml: Xml.Element = wrap(Xml.element("p").setChildren(Chunk(stub)))
    val (resolved, labels) = bib.resolve(xml)
    assert(labels.isEmpty)
    val last: Xml.Element = resolved.getChildren.flatMap(_.asElement).last
    assert(Citation.isList(last), render(resolved))
    assert(render(last).toLowerCase.contains("knuth"), render(last))
    assert(render(last).contains(s"""id="${Citation.entryId("knuth79")}""""), render(last))
    assert(render(resolved).contains(s"""href="${Citation.entryHref("knuth79")}""""), render(resolved))
  }

  test("FrontMatter reads bibliography and csl") {
    val frontMatter: FrontMatter = FrontMatter.parse(Some(
      """bibliography: refs.bib
        |csl: chicago-author-date
        |""".stripMargin
    )).toOption.get
    assert(frontMatter.bibliography.contains("refs.bib"))
    assert(frontMatter.csl.contains("chicago-author-date"))
  }

  test("Bibliography.load needs both file and style; path is relative to the document") {
    withTempDir: dir =>
      NioFiles.writeString(File(dir, "refs.bib").toPath, knuthLamportBib)
      val loaded: Bibliography = Bibliography.load(dir, Some("refs.bib"), Some("ieee"), Some("de-DE"))
      assert(loaded.contains("knuth79"))
      assert(loaded.style == "ieee")
      assert(loaded.lang == "de-DE")

      val noStyle: Bibliography = Bibliography.load(dir, Some("refs.bib"), None, None)
      assert(!noStyle.contains("knuth79"))

      val noPath: Bibliography = Bibliography.load(dir, None, Some("apa"), None)
      assert(!noPath.contains("knuth79"))

      val missingFile: Bibliography = Bibliography.load(dir, Some("nope.bib"), Some("apa"), None)
      assert(!missingFile.contains("knuth79"))
      val stub: Xml.Element = Citation.cite(Citation.Mode.Parenthetical, Seq(Citation.Item("knuth79")))
      val (replacements, list) = missingFile.format(Seq(stub))
      assert(list.isEmpty)
      assert(render(replacements(stub)).contains("unresolved-citation"))
  }
