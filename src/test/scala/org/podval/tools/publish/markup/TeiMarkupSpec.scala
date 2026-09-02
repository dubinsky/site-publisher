package org.podval.tools.publish.markup

import org.podval.tools.publish.site.PageErrorReporter
import org.podval.xml.{HtmlXmlDialect, Xml, XmlParser}
import org.scalatest.funsuite.AnyFunSuite

final class TeiMarkupSpec extends AnyFunSuite:
  private def parse(input: String): Xml.Element =
    XmlParser.parseXml(input).toOption.get

  private def processResult(input: String): (Xml.Element, Option[Xml.Element]) =
    TeiMarkup.process(parse(input), PageErrorReporter.Silent)

  private def process(input: String): Xml.Element =
    processResult(input)._1

  private def render(element: Xml.Element): String =
    HtmlXmlDialect.render(element).replaceAll("\\s+", " ").replace("= ", "=")

  test("titleStmt title is extracted as tei-title; teiHeader stays") {
    val (xml, title) = processResult(
      """<TEI>
        |  <teiHeader><fileDesc><titleStmt>
        |    <title>Doc title</title>
        |    <author>A</author>
        |  </titleStmt></fileDesc></teiHeader>
        |  <text><body><p>Hello</p></body></text>
        |</TEI>""".stripMargin
    )
    val dumped: String = render(xml)
    assert(title.exists(_.getName == "tei-title"), dumped)
    assert(title.exists(_.getText.contains("Doc title")), dumped)
    assert(xml.gather(el => Option.when(el.getName == "teiHeader")(el)).nonEmpty, dumped)
    assert(xml.gather(el => Option.when(el.getName == "titleStmt")(el)).nonEmpty, dumped)
    assert(xml.gather(el => Option.when(el.getName == "tei-title")(el)).isEmpty, dumped)
    assert(dumped.contains("Hello"), dumped)
    assert(dumped.contains("A"), dumped)
  }

  test("title type=main wins over a sibling title") {
    val (xml, title) = processResult(
      """<TEI>
        |  <teiHeader><fileDesc><titleStmt>
        |    <title>Alt</title>
        |    <title type="main">Main</title>
        |  </titleStmt></fileDesc></teiHeader>
        |  <text><body><p>x</p></body></text>
        |</TEI>""".stripMargin
    )
    assert(title.exists(_.getText.trim == "Main"), render(xml))
  }

  test("empty titleStmt yields no document title") {
    val (xml, title) = processResult(
      """<TEI>
        |  <teiHeader><fileDesc><titleStmt><author>?</author></titleStmt></fileDesc></teiHeader>
        |  <text><body><p>x</p></body></text>
        |</TEI>""".stripMargin
    )
    assert(title.isEmpty, render(xml))
  }

  test("bibl title and body head are not the document title") {
    val (xml, title) = processResult(
      """<TEI>
        |  <teiHeader><fileDesc><titleStmt><author>?</author></titleStmt></fileDesc></teiHeader>
        |  <text><body>
        |    <head>Not the title</head>
        |    <p>See <bibl><title>Papers</title></bibl>.</p>
        |  </body></text>
        |</TEI>""".stripMargin
    )
    val dumped: String = render(xml)
    assert(title.isEmpty, dumped)
    assert(dumped.contains("Not the title"), dumped)
    assert(dumped.contains("Papers"), dumped)
  }

  test("title text does not insert a space before a comma after an inline name") {
    val (_, title) = processResult(
      """<collection>
        |  <title>О <orgName>секте</orgName> и <persName>Зальмане Боруховиче</persName>, и о</title>
        |</collection>""".stripMargin
    )
    val text: String = title.map(_.getText).getOrElse("")
    assert(text.contains("Боруховиче, и о"), text)
    assert(!text.contains("Боруховиче ,"), text)
    assert(text.contains("О секте и"), text)
  }

  test("store and collection child title is extracted and stripped") {
    val (store, storeTitle) = processResult("""<store><title>Fund 109</title><p>x</p></store>""")
    assert(storeTitle.exists(_.getText.contains("Fund 109")), render(store))
    assert(store.gather(el => Option.when(el.getName == "tei-title" || el.getName == "title")(el)).isEmpty, render(store))
    val (collection, collectionTitle) = processResult("""<collection><title>Case 29</title><p>y</p></collection>""")
    assert(collectionTitle.exists(_.getText.contains("Case 29")), render(collection))
  }

  test("xi:include of a missing file is a reference, not XInclude expansion") {
    val input: String =
      """<store xmlns:xi="http://www.w3.org/2001/XInclude">
        |  <name lang="en" n="books"/>
        |  <by selector="book">
        |    <xi:include href="this-file-does-not-exist.xml"/>
        |    <xi:include href="books/book/derzhavin.xml"/>
        |  </by>
        |</store>""".stripMargin
    val parsed: Xml.Element = parse(input)
    val index = StoreIndex(parsed).get
    assert(index.hrefs == Seq("this-file-does-not-exist.xml", "books/book/derzhavin.xml"), index.hrefs)
    assert(index.selector.contains("book"), index.selector)
    assert(index.names.exists(_.n == "books"), index.names.map(_.n))
    val dumped: String = render(process(input))
    assert(!dumped.contains("xi:include"), dumped)
    assert(!dumped.contains("this-file-does-not-exist"), dumped)
    assert(!dumped.contains("book:"), dumped)
    assert(!dumped.contains("store-by"), dumped)
    assert(!dumped.contains("store-name"), dumped)
    assert(!dumped.contains("store-header"), dumped)
  }

  test("store names, title, and abstract are header chrome, not body") {
    val input: String =
      """<store>
        |  <name lang="ru" n="РГАДА"/>
        |  <name lang="en" n="rgada"/>
        |  <title>Российский государственный архив древних актов (Москва)</title>
        |  <abstract><ref target="http://example.com/">Вебсайт</ref></abstract>
        |  <by selector="category"/>
        |</store>""".stripMargin
    val parsed: Xml.Element = parse(input)
    val index = StoreIndex(parsed).get
    assert(index.displayName.contains("РГАДА"), index.displayName)
    assert(index.title.exists(_.getText.contains("Российский государственный архив")), index.title.map(_.getText))
    assert(index.description.exists(_.getText.contains("Вебсайт")), index.description.map(_.getText))
    val dumped: String = render(process(input))
    assert(!dumped.contains("store-name"), dumped)
    assert(!dumped.contains("Российский государственный архив"), dumped)
    assert(!dumped.contains("Вебсайт"), dumped)
    assert(!dumped.contains("store-by"), dumped)
    assert(!dumped.contains("category:"), dumped)
    assert(index.selector.contains("category"), index.selector)
  }

  test("store and collection harvest optional alias") {
    assert(StoreIndex(parse("""<store alias="rgada"><title>x</title></store>""")).get.alias.contains("rgada"))
    assert(StoreIndex(parse("""<collection n="3140"><title>x</title></collection>""")).get.alias.isEmpty)
  }

  test("collection harvests parts and pageType") {
    val index = StoreIndex(parse(
      """<collection pageType="book" n="6">
        |  <part n="1" from="084"><title>Materials</title></part>
        |  <part n="2" from="253.1"><title>End</title></part>
        |</collection>""".stripMargin
    )).get
    assert(index.pageType == PageType.Book, index.pageType)
    assert(index.parts.map(_.from) == Seq("084", "253.1"), index.parts.map(_.from))
    assert(index.parts.head.title.exists(_.getText.contains("Materials")), index.parts.head.title.map(_.getText))
  }

  test("pb becomes a facsimile anchor with p{n} id") {
    val dumped: String = render(process(
      """<TEI>
        |  <text><body><pb n="000-1"/><p>x</p></body></text>
        |</TEI>""".stripMargin
    ))
    assert(dumped.contains("""id="p000-1""""), dumped)
    assert(dumped.contains("""class="pb""""), dumped)
    assert(dumped.contains("""class="icon-span grey fa-classic fa-solid fa-images""""), dumped)
    assert(!dumped.contains("tei-class"), dumped)
    assert(!dumped.contains("⎙"), dumped)
  }

  test("document header harvests lang and pbs from the raw tree") {
    val header = DocumentHeader.harvest(parse(
      """<TEI>
        |  <teiHeader><fileDesc><titleStmt></titleStmt></fileDesc></teiHeader>
        |  <text xml:lang="he"><body>
        |    <pb n="001-1"/>
        |    <pb n="001-2" missing="true" empty="true"/>
        |  </body></text>
        |</TEI>""".stripMargin
    )).get
    assert(header.lang.contains("he"), header.lang)
    assert(header.pbs.map(_.n) == Seq("001-1", "001-2"), header.pbs.map(_.n))
    assert(!header.pbs.head.isMissing, header.pbs.head)
    assert(header.pbs.last.isMissing && header.pbs.last.isEmpty, header.pbs.last)
    assert(header.pbs.forall(_.facs.isEmpty), header.pbs.map(_.facs))
  }

  test("person has no document title") {
    val (xml, title) = processResult("""<person><persName>Zalman</persName></person>""")
    assert(title.isEmpty, render(xml))
  }

  test("person, place, and org roots are TEI") {
    assert(Markup.forElement("person").contains(TeiMarkup))
    assert(Markup.forElement("place").contains(TeiMarkup))
    assert(Markup.forElement("org").contains(TeiMarkup))
    assert(Markup.forElement("entityLists").contains(TeiMarkup))
    assert(Markup.forElement("TEI").contains(TeiMarkup))
  }

  test("entityLists document title is the direct child; list title becomes tei-head") {
    val (xml, title) = processResult(
      """<entityLists>
        |  <title>Имена</title>
        |  <listPerson n="jews" role="jew"><title>Жиды</title></listPerson>
        |  <listPlace n="places"><title>Места</title></listPlace>
        |</entityLists>""".stripMargin
    )
    val dumped: String = render(xml)
    assert(title.exists(_.getText.contains("Имена")), dumped)
    assert(xml.gather(el => Option.when(el.getName == "title" || el.getName == "tei-title")(el)).isEmpty, dumped)
    val people: Seq[Xml.Element] = xml.gather(el => Option.when(el.getName == "listPerson")(el)).toSeq
    assert(people.exists(_.getId.contains("jews")), dumped)
    assert(people.exists(el => el.getChildren.flatMap(_.asElement).exists(h =>
      h.getName == "tei-head" && h.getText.contains("Жиды")
    )), dumped)
    val index = EntityLists.harvest(parse(
      """<entityLists>
        |  <listPerson n="jews" role="jew"><title>Жиды</title></listPerson>
        |</entityLists>""".stripMargin
    )).get
    assert(index.lists.size == 1)
    assert(index.lists.head.id == "jews")
    assert(index.lists.head.role.contains("jew"))
    assert(index.lists.head.kind == EntityKind.Person)
  }

  test("entity name with ref becomes a; without ref stays") {
    val xml: Xml.Element = process(
      """<p>See <persName ref="alter-rebbe">the Rebbe</persName>
        | in <placeName ref="Вильна">Vilna</placeName>
        | and <orgName ref="кагал">the kahal</orgName>
        | vs <persName>bare</persName>
        | and <persName ref="  ">empty</persName>.</p>""".stripMargin
    )
    val dumped: String = render(xml)

    def named(cls: String, href: String): Seq[Xml.Element] =
      xml.gather(el =>
        Option.when(el.isA && el.hasClass(cls) && el.getHref.contains(href))(el)
      ).toSeq

    assert(named("persName", "alter-rebbe").exists(_.getText.contains("the Rebbe")), dumped)
    assert(named("placeName", "Вильна").exists(_.getText.contains("Vilna")), dumped)
    assert(named("orgName", "кагал").exists(_.getText.contains("the kahal")), dumped)

    val leftover: Seq[Xml.Element] = xml.gather(el =>
      Option.when(el.getName == "persName")(el)
    ).toSeq
    assert(leftover.exists(_.getText.contains("bare")), dumped)
    assert(leftover.exists(_.getText.contains("empty")), dumped)
    assert(leftover.forall(_.getHref.isEmpty), dumped)
    assert(xml.gather(el => Option.when(Citation.isCite(el))(el)).isEmpty, dumped)
  }

  test("endnote among mixed text and elements does not throw") {
    val xml: Xml.Element = process(
      """<p><note place="end">leading</note> after <hi>x</hi><note place="end">clung</note> tail</p>"""
    )
    val dumped: String = render(xml)
    assert(Footnote.linkIds(xml).size == 2, dumped)
    assert(dumped.contains("after"), dumped)
    assert(dumped.contains("tail"), dumped)
  }

  test("pb among whitespace does not throw") {
    val xml: Xml.Element = process(
      """<TEI>
        |  <teiHeader><fileDesc><titleStmt><author>?</author></titleStmt></fileDesc></teiHeader>
        |  <text xml:lang="ru"><body>
        |    <pb n="032-1"/>
        |    <pb n="032-2"/>
        |  </body></text>
        |</TEI>""".stripMargin
    )
    val dumped: String = render(xml)
    assert(dumped.contains("032-1"), dumped)
    assert(dumped.contains("032-2"), dumped)
  }

  test("note place=end becomes footnote IR; class is not tei-class; plain note stays") {
    val xml: Xml.Element = process(
      """<div>
        |<p>See this<note place="end">A note.</note>.</p>
        |<note>keep me</note>
        |</div>""".stripMargin
    )
    val dumped: String = render(xml)
    assert(dumped.contains("""class="footnote-link""""), dumped)
    assert(!dumped.contains("""tei-class="footnote-link""""), dumped)
    assert(dumped.contains("""class="footnote""""), dumped)
    assert(dumped.contains("A note"), dumped)
    val ids: Seq[String] = Footnote.linkIds(xml).toSeq
    assert(ids.size == 1, dumped)
    val bodies: Seq[Xml.Element] = xml.gather( element =>
      Option.when(Footnote.isBody(element))(element)
    ).toSeq
    assert(bodies.size == 1, dumped)
    assert(Footnote.getCorrelationId(bodies.head) == ids.head, dumped)
    val leftover: Seq[Xml.Element] = xml.gather( element =>
      Option.when(element.getName == "note")(element)
    ).toSeq
    assert(leftover.size == 1, dumped)
    assert(leftover.head.getText.contains("keep me"), dumped)
  }

  test("note after text or a preceding element has no separating HTML space") {
    def published(input: String, width: Int = 40): String =
      val xml: Xml.Element = process(input)
      val (notes, harvested) = Footnote.harvest(xml)
      val resolved: Xml.Element = harvested.transform(el => Footnote.resolveLink(el, notes, attachTip = true))
      HtmlXmlDialect.render(resolved, width)

    def compact(html: String): String = html.replaceAll("\\s+", " ").replace("= ", "=")

    def clings(html: String, before: String): Unit =
      val c: String = compact(html)
      assert(c.contains(s"""$before<span class="footnote-ref""""), html)
      assert(!c.contains(s"""$before <span class="footnote-ref""""), html)

    clings(published("""<p>See this<note place="end">A note.</note>.</p>"""), "this")
    clings(published("""<p>See <hi>this</hi><note place="end">A note.</note>.</p>"""), "</hi>")
    clings(
      published("""<p>See <ref target="#x">this</ref><note place="end">A note.</note>.</p>"""),
      "</a>"
    )
  }

  test("row/cell become tr/td and cols becomes colspan") {
    val xml: Xml.Element = process(
      """<table>
        |  <row role="label"><cell>A</cell><cell cols="2">B</cell></row>
        |  <row><cell>1</cell><cell>2</cell></row>
        |</table>""".stripMargin
    )
    val dumped: String = render(xml)
    assert(dumped.contains("<tr"), dumped)
    assert(dumped.contains("<td"), dumped)
    assert(dumped.contains("""colspan="2""""), dumped)
    assert(!dumped.contains("<row"), dumped)
    assert(!dumped.contains("<cell"), dumped)
    val cells: Seq[String] = xml.gather( element =>
      Option.when(element.getName == "td" || element.getName == "th")(element.getText.trim)
    ).toSeq.filter(_.nonEmpty)
    assert(cells.contains("A"), dumped)
    assert(cells.contains("B"), dumped)
    assert(cells.contains("1"), dumped)
    assert(cells.contains("2"), dumped)
  }

  test("list type=gloss becomes glossary IR; xml:id on label wins; unmarked list stays") {
    val xml: Xml.Element = process(
      """<div>
        |<p>See <ref target="#posuk">posuk</ref> and <term ref="#mud">mud</term>.</p>
        |<list type="gloss">
        |  <label xml:id="posuk">posuk</label>
        |  <item>verse</item>
        |  <label>akdamus</label>
        |  <label xml:id="rasha">rasha</label>
        |  <item>sinner</item>
        |</list>
        |<list type="glossary">
        |  <label>Alter Rebbe</label>
        |  <item xml:id="custom">the first Lubavitcher Rebbe</item>
        |  <label>mud</label>
        |  <item>wet dirt</item>
        |</list>
        |<list>
        |  <item>plain</item>
        |</list>
        |</div>""".stripMargin
    )
    val dumped: String = render(xml)
    assert(dumped.contains("glossary-item"), dumped)
    assert(!dumped.contains("""tei-class="glossary""""), dumped)
    assert(!dumped.contains("""tei-class="glossary-item""""), dumped)
    assert(dumped.contains("""href="#posuk""""), dumped)
    assert(dumped.contains("""href="#mud""""), dumped)
    val defs: Map[String, Xml.Nodes] = Glossary.definitions(xml)
    assert(defs.keySet == Set("posuk", "rasha", "custom", "mud"), dumped)
    assert(Xml.toString(defs("posuk")).trim == "verse")
    assert(Xml.toString(defs("rasha")).trim == "sinner")
    assert(Xml.toString(defs("custom")).trim == "the first Lubavitcher Rebbe")
    assert(Xml.toString(defs("mud")).trim == "wet dirt")
    val items: Set[String] = xml.gather( element =>
      Option.when(Glossary.isItem(element))(element.getId)
    ).flatten.toSet
    assert(items == Set("posuk", "akdamus", "rasha", "custom", "mud"), dumped)
    assert(dumped.contains("plain"), dumped)
    val glossLists: Seq[Xml.Element] = xml.gather( element =>
      Option.when(Glossary.isList(element))(element)
    ).toSeq
    assert(glossLists.size == 2, dumped)
    assert(glossLists.forall(_.getName == "dl"), dumped)
    val leftoverLists: Seq[Xml.Element] = xml.gather( element =>
      Option.when(element.getName == "list")(element)
    ).toSeq
    assert(leftoverLists.size == 1, dumped)
  }

  test("code lang becomes language-* class; multiline is wrapped in pre; eg stays") {
    val xml: Xml.Element = process(
      """<div>
        |<p>Use <code lang="scala">xs.map(f)</code> in</p>
        |<code lang="JAVA">Size s = new Size();
        |s.Width = 500;</code>
        |<code>plain</code>
        |<eg>not code</eg>
        |</div>""".stripMargin
    )
    val dumped: String = render(xml)
    assert(dumped.contains("""class="language-scala""""), dumped)
    assert(dumped.contains("""class="language-java""""), dumped)
    assert(dumped.contains("<pre"), dumped)
    assert(dumped.contains("xs.map(f)"), dumped)
    assert(dumped.contains("not code"), dumped)
    val codes: Seq[Xml.Element] = xml.gather( element =>
      Option.when(element.getName == "code")(element)
    ).toSeq
    assert(codes.exists(c => c.hasClass("language-scala") && !c.getText.contains('\n')), dumped)
    val pres: Seq[Xml.Element] = xml.gather( element =>
      Option.when(element.getName == "pre")(element)
    ).toSeq
    assert(pres.size == 1, dumped)
    val preCode: Xml.Element = pres.head.getChildren.flatMap(_.asElement).find(_.getName == "code").get
    assert(preCode.hasClass("language-java"), dumped)
    assert(preCode.getText.contains("Width"), dumped)
    val plain: Xml.Element = codes.find(c => !c.getClasses.exists(_.startsWith("language-"))).get
    assert(plain.getText.contains("plain"), dumped)
    val egs: Seq[Xml.Element] = xml.gather( element =>
      Option.when(element.getName == "eg")(element)
    ).toSeq
    assert(egs.size == 1, dumped)
  }

  test("listBibl entries get id and bibliography-item; empty ptr is labeled") {
    val xml: Xml.Element = process(
      """<div>
        |<p>See <ref target="#knuth79">Knuth 1979</ref> and <ptr target="#lamport94"/>.</p>
        |<listBibl>
        |  <head>References</head>
        |  <bibl xml:id="knuth79">Knuth, Donald E.</bibl>
        |  <biblStruct xml:id="lamport94"><title>LaTeX</title></biblStruct>
        |</listBibl>
        |</div>""".stripMargin
    )
    val dumped: String = render(xml)
    val lists: Seq[Xml.Element] = xml.gather(el => Option.when(BibliographyItem.isList(el))(el)).toSeq
    assert(lists.size == 1, dumped)
    val items: Seq[Xml.Element] = xml.gather(el => Option.when(BibliographyItem.isItem(el))(el)).toSeq
    assert(items.map(_.getId).toSet == Set(Some("knuth79"), Some("lamport94")), dumped)
    assert(!dumped.contains("""tei-class="bibliography""""), dumped)
    assert(!dumped.contains("""tei-class="bibliography-item""""), dumped)
    assert(dumped.contains("""href="#knuth79""""), dumped)
    assert(dumped.contains("Knuth 1979"), dumped)
    val ptr: Xml.Element = xml.gather(el =>
      Option.when(el.isA && el.getHref.contains("#lamport94"))(el)
    ).head
    assert(ptr.getText.trim == "lamport94", dumped)
    assert(xml.gather(el => Option.when(Citation.isCite(el))(el)).isEmpty, dumped)
  }

  test("standalone bibl and quote attribution are not bibliography items") {
    val xml: Xml.Element = process(
      """<div>
        |<p>See <bibl>Jefferson</bibl>.</p>
        |<cit><quote>quoted</quote><bibl xml:id="jeff">Jefferson</bibl></cit>
        |<listBibl><bibl xml:id="knuth79">Knuth</bibl></listBibl>
        |</div>""".stripMargin
    )
    val dumped: String = render(xml)
    val items: Seq[Xml.Element] = xml.gather(el => Option.when(BibliographyItem.isItem(el))(el)).toSeq
    assert(items.map(_.getId) == Seq(Some("knuth79")), dumped)
    assert(xml.gather(el => Option.when(Quote.is(el))(el)).size == 1, dumped)
  }

  test("cit without a quote is not a block quote") {
    val xml: Xml.Element = process(
      """<div><cit><ref target="#knuth79">Knuth</ref></cit>
        |<listBibl><bibl xml:id="knuth79">Knuth</bibl></listBibl></div>""".stripMargin
    )
    val dumped: String = render(xml)
    assert(xml.gather(el => Option.when(Quote.is(el))(el)).isEmpty, dumped)
    assert(dumped.contains("Knuth"), dumped)
  }

  test("cRef and bare target become citeproc stubs; #id to listBibl stays a link") {
    val xml: Xml.Element = process(
      """<div>
        |<p>See <ref target="#knuth79">Knuth 1979</ref>
        |and <ref cRef="lamport94"/>
        |and <ptr cRef="knuth79" n="p. 12"/>
        |and <ref target="missing-key"/>.</p>
        |<listBibl><bibl xml:id="knuth79">Knuth</bibl></listBibl>
        |<div type="bibliography"/>
        |</div>""".stripMargin
    )
    val dumped: String = render(xml)
    val items: Seq[Xml.Element] = xml.gather(el => Option.when(BibliographyItem.isItem(el))(el)).toSeq
    assert(items.map(_.getId) == Seq(Some("knuth79")), dumped)
    assert(dumped.contains("""href="#knuth79""""), dumped)
    assert(dumped.contains("Knuth 1979"), dumped)
    val stubs: Seq[Xml.Element] = xml.gather(el => Option.when(Citation.isCite(el))(el)).toSeq
    val stubItems: Seq[Citation.Item] = stubs.flatMap(Citation.itemsOf)
    assert(stubItems.map(_.key).toSet == Set("lamport94", "knuth79", "missing-key"), dumped)
    assert(stubItems.exists(item => item.key == "knuth79" && item.locator.contains("p. 12")), dumped)
    val placeholders: Seq[Xml.Element] =
      xml.gather(el => Option.when(Citation.isPlaceholder(el))(el)).toSeq
    assert(placeholders.size == 1, dumped)
  }

  test("bare target that matches a listBibl id becomes an internal #href") {
    val xml: Xml.Element = process(
      """<div><p><ptr target="knuth79"/></p>
        |<listBibl><bibl xml:id="knuth79">Knuth</bibl></listBibl></div>""".stripMargin
    )
    val dumped: String = render(xml)
    assert(xml.gather(el => Option.when(Citation.isCite(el))(el)).isEmpty, dumped)
    val ptr: Xml.Element = xml.gather(el =>
      Option.when(el.isA && el.getHref.contains("#knuth79"))(el)
    ).head
    assert(ptr.getText.trim == "knuth79", dumped)
  }

  test("listBibl in teiHeader is not document bibliography") {
    val xml: Xml.Element = process(
      """<TEI>
        |<teiHeader><fileDesc><sourceDesc>
        |<listBibl><bibl xml:id="header-only">Catalogue</bibl></listBibl>
        |</sourceDesc></fileDesc></teiHeader>
        |<text><body>
        |<listBibl><bibl xml:id="knuth79">Knuth</bibl></listBibl>
        |</body></text>
        |</TEI>""".stripMargin
    )
    val dumped: String = render(xml)
    val items: Seq[Xml.Element] = xml.gather(el => Option.when(BibliographyItem.isItem(el))(el)).toSeq
    assert(items.map(_.getId) == Seq(Some("knuth79")), dumped)
    assert(!items.exists(_.getId.contains("header-only")), dumped)
  }
