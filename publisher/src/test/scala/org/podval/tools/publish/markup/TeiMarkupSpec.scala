package org.podval.tools.publish.markup

import org.podval.tools.publish.site.{PageError, PageErrorReporter}
import org.podval.xml.{HtmlXmlWriterConfig, Xml, XmlParser, XmlElement}
import org.scalatest.funsuite.AnyFunSuite
import scala.collection.mutable

final class TeiMarkupSpec extends AnyFunSuite:
  private def parse(input: String): Xml.Element =
    XmlParser.parseXml(input).toOption.get

  private def processResult(input: String): (Xml.Element, Option[Xml.Element]) =
    TeiMarkup.process(parse(input), PageErrorReporter.Silent)

  private def process(input: String): Xml.Element =
    processResult(input)._1

  private final class Reporter extends PageErrorReporter:
    var errors: Seq[(PageError.Kind, String)] = Seq.empty
    override def error(
      kind: PageError.Kind,
      message: String,
      cause: Option[Throwable] = None
    ): Unit =
      errors = errors :+ (kind -> message)

  private def processReporting(input: String): (Xml.Element, Reporter) =
    val reporter: Reporter = Reporter()
    (TeiMarkup.process(parse(input), reporter)._1, reporter)

  private def withHands(body: String): String =
    s"""<TEI><teiHeader><fileDesc><titleStmt/></fileDesc>
       |<profileDesc>
       |<handNote xml:id="pencilHand" medium="pencil">записано карандашом</handNote>
       |<handNote xml:id="inkHand" medium="ink">записано чернилами</handNote>
       |<handNote xml:id="pencilStrike" medium="pencil">зачёркнуто карандашом</handNote>
       |</profileDesc></teiHeader>
       |<text><body>$body</body></text></TEI>""".stripMargin

  private def render(element: Xml.Element): String =
    HtmlXmlWriterConfig.render(element).replaceAll("\\s+", " ").replace("= ", "=")

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
    assert(title.exists(_.isNamed("tei-title")), dumped)
    assert(title.exists(_.getText.contains("Doc title")), dumped)
    assert(xml.gather(el => Option.when(el.isNamed("teiHeader"))(el)).nonEmpty, dumped)
    assert(xml.gather(el => Option.when(el.isNamed("titleStmt"))(el)).nonEmpty, dumped)
    assert(xml.gather(el => Option.when(el.isNamed("tei-title"))(el)).isEmpty, dumped)
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
    assert(store.gather(el => Option.when(el.isNamed("tei-title") || el.isElement(XmlElement.Title))(el)).isEmpty, render(store))
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
    assert(index.names.exists(_.name == "books"), index.names.map(_.name))
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
    assert(index.displayName("ru").contains("РГАДА"), index.displayName("ru"))
    assert(index.displayName("en").contains("rgada"), index.displayName("en"))
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
    val store: StoreIndex = StoreIndex(parse("""<store alias="rgada"><title>x</title></store>""")).get
    assert(store.alias.contains("rgada"))
    assert(!store.isCollection)
    val collection: StoreIndex = StoreIndex(parse("""<collection n="3140"><title>x</title></collection>""")).get
    assert(collection.alias.isEmpty)
    assert(collection.isCollection)
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

  test("collection part title keeps inline markup") {
    val index = StoreIndex(parse(
      """<collection>
        |  <part from="000"><title>the <hi>Rebbe</hi></title></part>
        |</collection>""".stripMargin
    )).get
    val title: Xml.Element = index.parts.head.title.get
    assert(title.childElements.map(_.getName.qName) == Seq("hi"), title.getChildren)
  }

  test("collection part title keeps TEI default xmlns") {
    val index = StoreIndex(parse(
      """<collection>
        |  <part from="000"><title xmlns="http://www.tei-c.org/ns/1.0">Первый арест</title></part>
        |</collection>""".stripMargin
    )).get
    val title: Xml.Element = index.parts.head.title.get
    assert(title.getText.contains("Первый арест"), title.getText)
  }

  test("body date with @when gets a calendar tooltip") {
    val dumped: String = render(process(
      """<TEI>
        |  <text><body><p>On <date when="1798-08-11">11 августа</date>.</p></body></text>
        |</TEI>""".stripMargin
    ))
    java.nio.file.Files.writeString(java.nio.file.Path.of("/tmp/tei-date-dump.html"), dumped)
    assert(dumped.contains("""class="date-ref""""), dumped)
    assert(dumped.contains("""<span class="date-tip">"""), dumped)
    assert(!dumped.contains("""tei-class="date-tip""""), dumped)
    assert(dumped.contains("11 августа"), dumped)
    assert(dumped.contains("1798 August 11"), dumped)
    assert(!dumped.contains("Julian"), dumped)
  }

  test("body date with notBefore and notAfter keeps the text and gets a tooltip") {
    val dumped: String = render(process(
      """<TEI>
        |  <text><body><p>On <date notBefore="1800-11-15" notAfter="1800-11-16">15 или 16 ноября 1800</date>.</p></body></text>
        |</TEI>""".stripMargin
    ))
    assert(dumped.contains("""<span class="date-tip">"""), dumped)
    assert(!dumped.contains("""tei-class="date-tip""""), dumped)
    assert(dumped.contains("15 или 16 ноября 1800"), dumped)
    assert(dumped.contains(">Not before<"), dumped)
    assert(dumped.contains(">Not after<"), dumped)
  }

  test("a page with no text before the next pb is a blank facsimile page") {
    val source: String =
      """<TEI>
        |  <teiHeader><fileDesc><titleStmt>
        |    <title>Header text</title>
        |    <pb n="header"/>
        |  </titleStmt></fileDesc></teiHeader>
        |  <text><body>
        |    <pb n="1"/>
        |    <pb n="2"/>
        |    <p>words</p>
        |    <pb n="3"/>
        |    <!-- note -->
        |    <pb n="4" missing="true"/>
        |    <pb n="5"/>
        |    <p> </p>
        |  </body></text>
        |</TEI>
        |""".stripMargin
    assert(Pb.noTranscription(parse(source)) == Set("1", "3", "5"))
    val dumped: String = render(process(source))
    assert(blankAttr(dumped, "p1"), dumped)
    assert(blankAttr(dumped, "p3"), dumped)
    assert(blankAttr(dumped, "p5"), dumped)
    assert(dumped.contains("""id="p2""""), dumped)
    assert(!blankAttr(dumped, "p2"), dumped)
    assert(dumped.contains("""id="p4""""), dumped)
    assert(!blankAttr(dumped, "p4"), dumped)
    assert(dumped.contains("""id="pheader""""), dumped)
    assert(!blankAttr(dumped, "pheader"), dumped)
  }

  private def blankAttr(page: String, id: String): Boolean =
    val marker: String = s"""id="$id""""
    val start: Int = page.indexOf(marker)
    if start < 0 then false
    else page.substring(start, page.indexOf(">", start) + 1).contains("data-facsimile-blank")

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
    assert(xml.gather(el => Option.when(el.isElement(XmlElement.Title) || el.isNamed("tei-title"))(el)).isEmpty, dumped)
    val people: Seq[Xml.Element] = xml.gather(el => Option.when(el.isNamed("listPerson"))(el)).toSeq
    assert(people.exists(_.getId.contains("jews")), dumped)
    assert(people.exists(el => el.childElements.exists(h =>
      h.isNamed("tei-head") && h.getText.contains("Жиды")
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

  test("entity list head is the list title and renders as tei-head") {
    val (xml, _) = processResult(
      """<entityLists>
        |  <listPerson n="jews" role="jew"><head>Жиды</head></listPerson>
        |</entityLists>""".stripMargin
    )
    val dumped: String = render(xml)
    assert(xml.gather(el => Option.when(el.isNamed("tei-head") && el.getText.contains("Жиды"))(el)).nonEmpty, dumped)
    val index = EntityLists.harvest(parse(
      """<entityLists xmlns="http://www.tei-c.org/ns/1.0">
        |  <listPerson n="jews"><head>Жиды</head></listPerson>
        |</entityLists>""".stripMargin
    )).get
    assert(index.lists.head.title == "Жиды")
  }

  test("editor role translator is harvested apart from transcriber") {
    val header = DocumentHeader.harvest(parse(
      """<TEI><teiHeader><fileDesc><titleStmt>
        |  <editor role="transcriber"><persName>Reader</persName></editor>
        |  <editor role="translator"><persName>Переводчик</persName></editor>
        |</titleStmt></fileDesc></teiHeader>
        |<text><body/></text></TEI>""".stripMargin
    )).get
    assert(header.transcribers.map(_.getText) == Seq("Reader"))
    assert(header.translators.map(_.getText) == Seq("Переводчик"))
    assert(!header.isEmpty)
  }

  test("correspAction type received is the addressee") {
    val header = DocumentHeader.harvest(parse(
      """<TEI><teiHeader><fileDesc><titleStmt/></fileDesc>
        |<profileDesc><correspDesc>
        |  <correspAction type="sent"><persName>Writer</persName></correspAction>
        |  <correspAction type="Received"><persName>Recipient</persName></correspAction>
        |</correspDesc></profileDesc></teiHeader>
        |<text><body/></text></TEI>""".stripMargin
    )).get
    assert(header.addressee.exists(_.getText == "Recipient"))
  }

  test("addressee role is used when no action is received") {
    val header = DocumentHeader.harvest(parse(
      """<TEI><teiHeader><fileDesc><titleStmt/></fileDesc>
        |<profileDesc><correspDesc>
        |  <correspAction type="sent"><persName role="addressee">Fallback</persName></correspAction>
        |</correspDesc></profileDesc></teiHeader>
        |<text><body/></text></TEI>""".stripMargin
    )).get
    assert(header.addressee.exists(_.getText == "Fallback"))
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
      Option.when(el.isNamed("persName"))(el)
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
      Option.when(element.isNamed("note"))(element)
    ).toSeq
    assert(leftover.size == 1, dumped)
    assert(leftover.head.getText.contains("keep me"), dumped)
  }

  test("note after text or a preceding element has no separating HTML space") {
    def published(input: String, width: Int = 40): String =
      val xml: Xml.Element = process(input)
      val (notes, harvested) = Footnote.harvest(xml)
      val resolved: Xml.Element = harvested.transform(el =>
        Footnote.resolveLink(el, notes, notes, attachTip = true, PageErrorReporter.Silent)
      )
      HtmlXmlWriterConfig.render(resolved, width)

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

  test("note @n is the visible marker; blank @n keeps the series number") {
    val xml: Xml.Element = process(
      """<div>
        |<p>A<note place="end" n="*">star</note> B<note place="end" n="3">three</note>
        |C<note place="end">plain</note> D<note place="end" n="  ">blank</note>.</p>
        |<note n="nope">not an endnote</note>
        |</div>""".stripMargin
    )
    val finished: Xml.Element = Footnote.finish(xml, PageErrorReporter.Silent)
    val dumped: String = render(finished)
    assert(dumped.contains("""id="_footnote_1""""), dumped)
    assert(dumped.contains("""id="_footnote_4""""), dumped)
    assert(!dumped.contains("""id="_footnote_5""""), dumped)
    assert(!dumped.contains("footnote-label"), dumped)
    val links: Seq[String] = finished.gather(el =>
      Option.when(el.hasClass("footnote-link"))(el.getText.trim)
    )
    assert(links == Seq("*", "3", "3", "4"), dumped)
    val backs: Seq[String] = finished.gather(el =>
      Option.when(el.hasClass("footnote-backlink"))(el.getText.trim)
    )
    assert(backs == Seq("*", "3", "3", "4"), dumped)
    val leftover: Seq[Xml.Element] = finished.gather(el =>
      Option.when(el.isNamed("note"))(el)
    )
    assert(leftover.exists(_.getText.contains("not an endnote")), dumped)
    assert(leftover.exists(_.get("n").contains("nope")), dumped)
  }

  test("note @n in a table or a parent note keeps the series id") {
    val table: Xml.Element = Footnote.finish(
      process("""<table><row><cell>x<note place="end" n="*">cell</note></cell></row></table>"""),
      PageErrorReporter.Silent,
      localTables = true
    )
    val tableDumped: String = render(table)
    assert(tableDumped.contains("""id="_table_1_fn_a""""), tableDumped)
    assert(tableDumped.contains("""id="_table_1_fn_src_a""""), tableDumped)
    assert(!tableDumped.contains("footnote-label"), tableDumped)
    val tableLinks: Seq[String] = table.gather(el =>
      Option.when(el.hasClass("footnote-link"))(el.getText.trim)
    )
    assert(tableLinks == Seq("*"), tableDumped)

    val nested: Xml.Element = Footnote.finish(
      process("""<p>See<note place="end" n="126">outer<note place="end" n="iv">inner</note></note>.</p>"""),
      PageErrorReporter.Silent
    )
    val nestedDumped: String = render(nested)
    assert(nestedDumped.contains("""id="_footnote_1""""), nestedDumped)
    assert(nestedDumped.contains("""id="_footnote_1_n_a""""), nestedDumped)
    assert(!nestedDumped.contains("""id="_footnote_2""""), nestedDumped)
    assert(!nestedDumped.contains("footnote-label"), nestedDumped)
    val outerSrc: Seq[String] = nested.gather(el =>
      Option.when(el.getId.contains("_footnote_src_1"))(el.getText.trim)
    )
    assert(outerSrc == Seq("126"), nestedDumped)
    val innerSrc: Seq[String] = nested.gather(el =>
      Option.when(el.getId.contains("_footnote_1_n_src_a"))(el.getText.trim)
    )
    assert(innerSrc == Seq("iv"), nestedDumped)
  }

  test("place=end in a table cell becomes a table-local letter") {
    val xml: Xml.Element = process(
      """<table>
        |  <row><cell>x<note place="end">cell note</note></cell></row>
        |</table>""".stripMargin
    )
    val dumpedIr: String = render(xml)
    assert(Footnote.linkIds(xml).size == 1, dumpedIr)
    assert(dumpedIr.contains("""class="footnote-link""""), dumpedIr)
    val finished: Xml.Element = Footnote.finish(xml, PageErrorReporter.Silent, localTables = true)
    val dumped: String = render(finished)
    assert(dumped.contains("table-with-notes"), dumped)
    assert(dumped.contains("""id="_table_1_fn_a""""), dumped)
    assert(dumped.contains("""data-footnote-scope="table""""), dumped)
    assert(dumped.contains("cell note"), dumped)
    assert(!dumped.contains("""id="_footnote_1""""), dumped)
  }

  test("nested place=end in place=end becomes a per-parent letter series") {
    val xml: Xml.Element = process(
      """<p>See<note place="end">outer<note place="end">inner</note></note>.</p>"""
    )
    val finished: Xml.Element = Footnote.finish(xml, PageErrorReporter.Silent)
    val dumped: String = render(finished)
    assert(dumped.contains("""id="_footnote_1""""), dumped)
    assert(dumped.contains("""id="_footnote_1_n_a""""), dumped)
    assert(dumped.contains("""data-footnote-scope="nested""""), dumped)
    assert(dumped.contains("nested-footnotes"), dumped)
    assert(dumped.contains("inner"), dumped)
    assert(!dumped.contains("""id="_footnote_2""""), dumped)
  }

  test("gloss containing place=end stays document-scope") {
    val xml: Xml.Element = process(
      """<p>See <note type="gloss">term<note place="end">expansion</note></note>.</p>"""
    )
    val finished: Xml.Element = Footnote.finish(xml, PageErrorReporter.Silent)
    val dumped: String = render(finished)
    assert(dumped.contains("""id="_footnote_1""""), dumped)
    assert(dumped.contains("expansion"), dumped)
    assert(!dumped.contains("nested-footnotes"), dumped)
    assert(!dumped.contains("""data-footnote-scope="nested""""), dumped)
    assert(!dumped.contains("_footnote_1_n_"), dumped)
    val leftover: Seq[Xml.Element] = finished.gather(el =>
      Option.when(el.isNamed("note"))(el)
    )
    assert(leftover.exists(_.getText.contains("term")), dumped)
  }

  test("place=foot is a source series separate from place=end") {
    val xml: Xml.Element = process(
      """<p>A<note place="foot">source</note> B<note place="end">encoder</note>
        |C<note place="foot" n="*">star</note> D<note>plain</note>.</p>""".stripMargin
    )
    assert(Footnote.linkIds(xml).size == 3, render(xml))
    val finished: Xml.Element = Footnote.finish(xml, PageErrorReporter.Silent)
    val dumped: String = render(finished)
    assert(dumped.contains("""id="_source_fn_1""""), dumped)
    assert(dumped.contains("""id="_source_fn_2""""), dumped)
    assert(dumped.contains("""id="_footnote_1""""), dumped)
    assert(!dumped.contains("""id="_footnote_2""""), dumped)
    assert(!dumped.contains("""id="_source_fn_3""""), dumped)
    assert(dumped.contains("source-footnotes"), dumped)
    assert(dumped.contains("""data-footnote-series="foot""""), dumped)
    assert(dumped.indexOf("source-footnotes") < dumped.indexOf("""id="_footnote_1""""), dumped)
    val links: Seq[String] = finished.gather(el =>
      Option.when(el.hasClass("footnote-link"))(el.getText.trim)
    )
    assert(links == Seq("1", "1", "*"), dumped)
    val leftover: Seq[Xml.Element] = finished.gather(el =>
      Option.when(el.isNamed("note"))(el)
    )
    assert(leftover.exists(_.getText.contains("plain")), dumped)
  }

  test("place=foot in a table is a table-local source letter") {
    val finished: Xml.Element = Footnote.finish(
      process("""<table><row><cell>x<note place="foot">cell</note></cell></row></table>"""),
      PageErrorReporter.Silent,
      localTables = true
    )
    val dumped: String = render(finished)
    assert(dumped.contains("""id="_table_1_sfn_a""""), dumped)
    assert(dumped.contains("source-footnotes"), dumped)
    assert(dumped.contains("table-footnotes"), dumped)
    assert(!dumped.contains("""id="_source_fn_1""""), dumped)
    assert(!dumped.contains("""id="_footnote_1""""), dumped)
  }

  test("place=end inside place=foot is a nested endnote letter") {
    val finished: Xml.Element = Footnote.finish(
      process("""<p>See<note place="foot">Outer<note place="end">Inner.</note></note>.</p>"""),
      PageErrorReporter.Silent
    )
    val dumped: String = render(finished)
    assert(dumped.contains("""id="_source_fn_1""""), dumped)
    assert(dumped.contains("""id="_source_fn_1_n_a""""), dumped)
    assert(!dumped.contains("""id="_footnote_1""""), dumped)
    assert(dumped.contains("Inner."), dumped)
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
      Option.when(element.isElement(XmlElement.Td) || element.isElement(XmlElement.Th))(element.getText.trim)
    ).toSeq.filter(_.nonEmpty)
    assert(cells.contains("A"), dumped)
    assert(cells.contains("B"), dumped)
    assert(cells.contains("1"), dumped)
    assert(cells.contains("2"), dumped)
  }

  test("cell rows greater than 1 becomes rowspan of the same integer") {
    val xml: Xml.Element = process(
      """<table>
        |  <row><cell rows="2" cols="2">A</cell></row>
        |  <row><cell rows="02">B</cell></row>
        |</table>""".stripMargin
    )
    val dumped: String = render(xml)
    assert(dumped.contains("""rowspan="2""""), dumped)
    assert(!dumped.contains("""rowspan="3""""), dumped)
    assert(dumped.contains("""colspan="2""""), dumped)
    assert(dumped.contains("""rows="2""""), dumped)
    assert(dumped.contains("""rows="02""""), dumped)
    assert(xml.getText.contains("A"), dumped)
    assert(xml.getText.contains("B"), dumped)
  }

  test("cell rows of 1, 0, or non-numeric does not set rowspan") {
    def dumped(rows: String): String =
      render(process(s"""<table><row><cell rows="$rows">A</cell></row></table>"""))
    val one: String = dumped("1")
    val zero: String = dumped("0")
    val foo: String = dumped("foo")
    assert(!one.contains("rowspan"), one)
    assert(one.contains("""rows="1""""), one)
    assert(!zero.contains("rowspan"), zero)
    assert(zero.contains("""rows="0""""), zero)
    assert(!foo.contains("rowspan"), foo)
    assert(foo.contains("""rows="foo""""), foo)
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
    assert(glossLists.forall(_.isElement(XmlElement.Dl)), dumped)
    val leftoverLists: Seq[Xml.Element] = xml.gather( element =>
      Option.when(element.isNamed("list"))(element)
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
      Option.when(element.isElement(XmlElement.Code))(element)
    ).toSeq
    assert(codes.exists(c => c.hasClass("language-scala") && !c.getText.contains('\n')), dumped)
    val pres: Seq[Xml.Element] = xml.gather( element =>
      Option.when(element.isElement(XmlElement.Pre))(element)
    ).toSeq
    assert(pres.size == 1, dumped)
    val preCode: Xml.Element = pres.head.childElements.find(_.isElement(XmlElement.Code)).get
    assert(preCode.hasClass("language-java"), dumped)
    assert(preCode.getText.contains("Width"), dumped)
    val plain: Xml.Element = codes.find(c => !c.getClasses.exists(_.startsWith("language-"))).get
    assert(plain.getText.contains("plain"), dumped)
    val egs: Seq[Xml.Element] = xml.gather( element =>
      Option.when(element.isNamed("eg"))(element)
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

  test("gap with reason gets a hover tip; empty reason does not") {
    val xml: Xml.Element = process(
      """<TEI><teiHeader><fileDesc><titleStmt/></fileDesc></teiHeader>
        |<text><body><p>a <gap reason="lost"/> b <gap reason=""/> c <gap/> d</p></body></text></TEI>""".stripMargin
    )
    val dumped: String = render(xml)
    val refs: Seq[Xml.Element] = xml.gather(el => Option.when(TeiGap.tip.isRef(el))(el)).toSeq
    assert(refs.size == 1, dumped)
    val tips: Seq[Xml.Element] = xml.gather(el => Option.when(el.has(TeiGap.tip.TipClass))(el)).toSeq
    assert(tips.size == 1, dumped)
    assert(tips.head.getText == "lost", dumped)
    assert(dumped.contains("""class="gap-ref""""), dumped)
    assert(dumped.contains("""class="gap-tip""""), dumped)
    assert(!dumped.contains("""tei-class="gap-tip""""), dumped)
    assert(dumped.contains("""reason="lost""""), dumped)
    assert(dumped.contains("<gap></gap>"), dumped)
  }

  test("gap tip wrap is not repeated on a second convert") {
    val once: Xml.Element = TeiGap.convert(parse("""<gap reason="illegible"/>"""))
    val twice: Xml.Element = TeiGap.convert(once)
    val dumped: String = render(twice)
    assert(render(once) == dumped, dumped)
    val refs: Seq[Xml.Element] = twice.gather(el => Option.when(TeiGap.tip.isRef(el))(el)).toSeq
    assert(refs.size == 1, dumped)
  }

  test("hand with a pencil note gets a medium mark and a hover tip") {
    val (xml, reporter) = processReporting(withHands(
      """<fw type="pageNum" place="right" hand="#pencilHand">6</fw>"""
    ))
    val dumped: String = render(xml)
    assert(reporter.errors.isEmpty, reporter.errors)
    val refs: Seq[Xml.Element] = xml.gather(el => Option.when(TeiHand.tip.isRef(el))(el)).toSeq
    assert(refs.size == 1, dumped)
    val fw: Xml.Element = refs.head.childElements.head
    val tip: Xml.Element = refs.head.childElements.drop(1).head
    assert(fw.isNamed("fw"), dumped)
    assert(fw.getText.trim == "6", dumped)
    assert(fw.get("type").contains("pageNum"), dumped)
    assert(fw.get("place").contains("right"), dumped)
    assert(fw.get("hand").contains("#pencilHand"), dumped)
    assert(fw.get("data-medium").contains("pencil"), dumped)
    assert(fw.get("data-hand-tip").contains("true"), dumped)
    assert(tip.getText == "записано карандашом", dumped)
    assert(tip.childElements.isEmpty, dumped)
    assert(dumped.contains("""class="hand-ref""""), dumped)
    assert(dumped.contains("""class="hand-tip""""), dumped)
    assert(!dumped.contains("""tei-class="hand-tip""""), dumped)
    assert(!dumped.contains("aria-describedby"), dumped)
  }

  test("nested ink writing and a pencil strike nest hand tips") {
    val xml: Xml.Element = process(withHands(
      """<fw type="pageNum" place="right" hand="#inkHand"><del rend="overstrike" hand="#pencilStrike">word</del></fw>"""
    ))
    val dumped: String = render(xml)
    val refs: Seq[Xml.Element] = xml.gather(el => Option.when(TeiHand.tip.isRef(el))(el)).toSeq
    assert(refs.size == 2, dumped)
    val fw: Xml.Element = refs.head.childElements.head
    assert(fw.isNamed("fw"), dumped)
    assert(fw.get("data-medium").contains("ink"), dumped)
    assert(fw.get("data-hand-tip").contains("true"), dumped)
    assert(fw.get("hand").contains("#inkHand"), dumped)
    val inner: Xml.Element = fw.childElements.head
    assert(TeiHand.tip.isRef(inner), dumped)
    val del: Xml.Element = inner.childElements.head
    assert(del.isNamed("del"), dumped)
    assert(del.get("rend").contains("overstrike"), dumped)
    assert(del.get("hand").contains("#pencilStrike"), dumped)
    assert(del.get("data-medium").contains("pencil"), dumped)
    assert(del.get("data-hand-tip").contains("true"), dumped)
    val tips: Seq[Xml.Element] = xml.gather(el => Option.when(el.has(TeiHand.tip.TipClass))(el)).toSeq
    assert(tips.map(_.getText) == Seq("зачёркнуто карандашом", "записано чернилами"), dumped)
    assert(dumped.contains("""class="hand-tip""""), dumped)
    assert(!dumped.contains("""tei-class="hand-tip""""), dumped)
  }

  test("hand tip wrap is not repeated on a second convert") {
    val hands: Map[String, TeiHand.Hand] = TeiHand.collect(parse(
      """<handNote xml:id="pencilHand" medium="pencil">записано карандашом</handNote>"""
    ))
    val once: Xml.Element = TeiHand.convert(
      parse("""<fw type="pageNum" hand="#pencilHand">6</fw>"""),
      hands,
      PageErrorReporter.Silent,
      mutable.Set.empty
    )
    val twice: Xml.Element = TeiHand.convert(once, hands, PageErrorReporter.Silent, mutable.Set.empty)
    val reentered: Xml.Element = DialectWalk.transform(once)(el =>
      TeiHand.convert(el, hands, PageErrorReporter.Silent, mutable.Set.empty)
    )
    val dumped: String = render(reentered)
    assert(render(once) == render(twice), dumped)
    assert(render(once) == dumped, dumped)
    val refs: Seq[Xml.Element] = reentered.gather(el => Option.when(TeiHand.tip.isRef(el))(el)).toSeq
    assert(refs.size == 1, dumped)
  }

  test("unresolved @hand is not wrapped and is reported once") {
    val (xml, reporter) = processReporting(
      """<TEI><teiHeader><fileDesc><titleStmt/></fileDesc>
        |<profileDesc><handNote xml:id="pencilHand" medium="pencil">записано карандашом</handNote></profileDesc>
        |</teiHeader><text><body>
        |<fw hand="#missing">1</fw> <fw hand="#missing">2</fw>
        |<fw hand="#other">3</fw> <fw hand="#other">4</fw>
        |</body></text></TEI>""".stripMargin
    )
    val dumped: String = render(xml)
    assert(xml.gather(el => Option.when(TeiHand.tip.isRef(el))(el)).isEmpty, dumped)
    assert(!dumped.contains("data-hand-tip"), dumped)
    assert(!dumped.contains("data-medium"), dumped)
    assert(dumped.contains("""hand="#missing""""), dumped)
    assert(dumped.contains("""hand="#other""""), dumped)
    assert(reporter.errors.map((kind, message) => (kind.id, message)) == Seq(
      "unresolved-hand" -> "#missing",
      "unresolved-hand" -> "#other"
    ), reporter.errors)
    assert(PageError.UnresolvedHand.id == "unresolved-hand")
    assert(PageError.all.contains(PageError.UnresolvedHand))
  }

  test("ill-formed @hand is not wrapped and is reported") {
    val (xml, reporter) = processReporting(
      """<TEI><text><body>
        |<fw hand="pencilHand">1</fw> <fw hand="pencilHand">2</fw>
        |<fw hand="#a #b">3</fw> <fw hand="#a #b">4</fw>
        |</body></text></TEI>""".stripMargin
    )
    val dumped: String = render(xml)
    assert(xml.gather(el => Option.when(TeiHand.tip.isRef(el))(el)).isEmpty, dumped)
    assert(!dumped.contains("data-hand-tip"), dumped)
    assert(dumped.contains("""hand="pencilHand""""), dumped)
    assert(dumped.contains("""hand="#a #b""""), dumped)
    assert(reporter.errors.map((kind, message) => (kind.id, message)) == Seq(
      "unresolved-hand" -> "pencilHand",
      "unresolved-hand" -> "#a #b"
    ), reporter.errors)
  }

  test("blank @hand is unchanged") {
    val (xml, reporter) = processReporting(
      """<TEI><text><body><fw hand="">1</fw><fw hand="  ">2</fw><fw>3</fw></body></text></TEI>"""
    )
    val dumped: String = render(xml)
    assert(reporter.errors.isEmpty, reporter.errors)
    assert(xml.gather(el => Option.when(TeiHand.tip.isRef(el))(el)).isEmpty, dumped)
    assert(!dumped.contains("data-hand-tip"), dumped)
  }

  test("empty hand text sets data-medium without a tip; empty note does nothing") {
    val (xml, reporter) = processReporting(
      """<TEI><teiHeader><fileDesc><titleStmt/></fileDesc><profileDesc>
        |<handNote xml:id="pencilOnly" medium="pencil"></handNote>
        |<handNote xml:id="blank"></handNote>
        |<handNote xml:id="spaces" medium="  ">   </handNote>
        |</profileDesc></teiHeader><text><body>
        |<fw hand="#pencilOnly">1</fw><fw hand="#blank">2</fw><fw hand="#spaces">3</fw>
        |</body></text></TEI>""".stripMargin
    )
    val dumped: String = render(xml)
    assert(reporter.errors.isEmpty, reporter.errors)
    assert(xml.gather(el => Option.when(TeiHand.tip.isRef(el))(el)).isEmpty, dumped)
    assert(!dumped.contains("data-hand-tip"), dumped)
    val fws: Seq[Xml.Element] = xml.gather(el => Option.when(el.isNamed("fw"))(el)).toSeq
    assert(fws.size == 3, dumped)
    assert(fws.head.get("data-medium").contains("pencil"), dumped)
    assert(fws.head.get("hand").contains("#pencilOnly"), dumped)
    assert(fws.drop(1).forall(_.get("data-medium").isEmpty), dumped)
  }

  test("unclear with reason gets a hover tip and keeps the reading") {
    val xml: Xml.Element = process("""<p>a <unclear reason="torn">word</unclear> b</p>""")
    val dumped: String = render(xml)
    val refs: Seq[Xml.Element] = xml.gather(el => Option.when(TeiUnclear.tip.isRef(el))(el)).toSeq
    assert(refs.size == 1, dumped)
    val unclear: Xml.Element = refs.head.childElements.head
    val tip: Xml.Element = refs.head.childElements.drop(1).head
    assert(unclear.isNamed("unclear"), dumped)
    assert(unclear.getText == "word", dumped)
    assert(tip.getText == "torn", dumped)
    assert(dumped.contains("""class="unclear-ref""""), dumped)
    assert(dumped.contains("""class="unclear-tip""""), dumped)
    assert(!dumped.contains("""tei-class="unclear-tip""""), dumped)
    assert(dumped.contains("""reason="torn""""), dumped)
  }

  test("unclear cert is lowercased onto data-cert and tipped") {
    val xml: Xml.Element = process("""<p><unclear cert="Medium">word</unclear></p>""")
    val dumped: String = render(xml)
    val unclear: Xml.Element = xml.gather(el => Option.when(el.isNamed("unclear"))(el)).head
    assert(unclear.get("data-cert").contains("medium"), dumped)
    assert(unclear.get("cert").contains("Medium"), dumped)
    val tips: Seq[Xml.Element] = xml.gather(el => Option.when(el.has(TeiUnclear.tip.TipClass))(el)).toSeq
    assert(tips.map(_.getText) == Seq("medium"), dumped)
  }

  test("unclear cert and reason share one tip") {
    val xml: Xml.Element = process("""<p><unclear cert="medium" reason="torn edge">word</unclear></p>""")
    val dumped: String = render(xml)
    val unclear: Xml.Element = xml.gather(el => Option.when(el.isNamed("unclear"))(el)).head
    assert(unclear.get("data-cert").contains("medium"), dumped)
    val tips: Seq[Xml.Element] = xml.gather(el => Option.when(el.has(TeiUnclear.tip.TipClass))(el)).toSeq
    assert(tips.map(_.getText) == Seq("medium — torn edge"), dumped)
  }

  test("absent, empty, or whitespace unclear attributes do not wrap") {
    val xml: Xml.Element = process(
      """<p><unclear>a</unclear> <unclear cert="">b</unclear> <unclear cert="  " reason=" ">c</unclear></p>"""
    )
    val dumped: String = render(xml)
    assert(xml.gather(el => Option.when(TeiUnclear.tip.isRef(el))(el)).isEmpty, dumped)
    assert(!dumped.contains("data-cert"), dumped)
    assert(!dumped.contains("data-unclear-tip"), dumped)
  }

  test("unclear tip wrap is not repeated on a second convert") {
    val once: Xml.Element = TeiUnclear.convert(
      parse("""<unclear cert="low" reason="torn">word</unclear>"""),
      Map.empty,
      PageErrorReporter.Silent,
      mutable.Set.empty
    )
    val twice: Xml.Element = TeiUnclear.convert(once, Map.empty, PageErrorReporter.Silent, mutable.Set.empty)
    val reentered: Xml.Element = DialectWalk.transform(once)(el =>
      TeiUnclear.convert(el, Map.empty, PageErrorReporter.Silent, mutable.Set.empty)
    )
    val dumped: String = render(reentered)
    assert(render(once) == render(twice), dumped)
    assert(render(once) == dumped, dumped)
    val refs: Seq[Xml.Element] = reentered.gather(el => Option.when(TeiUnclear.tip.isRef(el))(el)).toSeq
    assert(refs.size == 1, dumped)
  }

  test("misspelled cert is stored and tipped and stays an unclear page error") {
    val input: String = """<p><unclear cert="medum">word</unclear></p>"""
    val xml: Xml.Element = process(input)
    val dumped: String = render(xml)
    val unclear: Xml.Element = xml.gather(el => Option.when(el.isNamed("unclear"))(el)).head
    assert(unclear.get("data-cert").contains("medum"), dumped)
    val tips: Seq[Xml.Element] = xml.gather(el => Option.when(el.has(TeiUnclear.tip.TipClass))(el)).toSeq
    assert(tips.map(_.getText) == Seq("medum"), dumped)
    val reporter: Reporter = Reporter()
    TeiMarkup.reportSourceErrors(parse(input), reporter, namesWithoutRef = false)
    assert(reporter.errors.map((kind, _) => kind.id) == Seq("unclear"), reporter.errors)
    assert(reporter.errors.map(_._2) == Seq("word (cert: medum)"), reporter.errors)
  }

  test("unclear page error names cert and reason from the source text") {
    val reporter: Reporter = Reporter()
    TeiMarkup.reportSourceErrors(
      parse(
        """<div>
          |<unclear>plain</unclear>
          |<unclear cert="Medium">c</unclear>
          |<unclear reason=" torn ">r</unclear>
          |<unclear cert="low" reason="edge">b</unclear>
          |</div>""".stripMargin
      ),
      reporter,
      namesWithoutRef = false
    )
    assert(reporter.errors.map(_._1.id).forall(_ == "unclear"), reporter.errors)
    assert(reporter.errors.map(_._2) == Seq(
      "plain",
      "c (cert: Medium)",
      "r (reason: torn)",
      "b (cert: low; reason: edge)"
    ), reporter.errors)
  }

  test("unclear with cert and hand nests the hand tip inside the unclear tip") {
    val xml: Xml.Element = process(withHands(
      """<unclear cert="low" hand="#pencilHand">word</unclear>"""
    ))
    val dumped: String = render(xml)
    val unclearRefs: Seq[Xml.Element] = xml.gather(el => Option.when(TeiUnclear.tip.isRef(el))(el)).toSeq
    assert(unclearRefs.size == 1, dumped)
    val handRef: Xml.Element = unclearRefs.head.childElements.head
    assert(TeiHand.tip.isRef(handRef), dumped)
    assert(handRef.gather(el => Option.when(TeiUnclear.tip.isRef(el))(el)).isEmpty, dumped)
    val unclear: Xml.Element = handRef.childElements.head
    assert(unclear.isNamed("unclear"), dumped)
    assert(unclear.getText == "word", dumped)
    assert(unclear.get("data-cert").contains("low"), dumped)
    assert(unclear.get("data-medium").contains("pencil"), dumped)
    assert(unclear.get("data-unclear-tip").contains("true"), dumped)
    assert(unclear.get("data-hand-tip").contains("true"), dumped)
    val unclearTip: Xml.Element = unclearRefs.head.childElements.drop(1).head
    val handTip: Xml.Element = handRef.childElements.drop(1).head
    assert(unclearTip.getText == "low", dumped)
    assert(handTip.getText == "записано карандашом", dumped)
  }

  test("hand-only unclear gets the hand tip and no unclear tip") {
    val xml: Xml.Element = process(withHands(
      """<unclear hand="#pencilHand">word</unclear>"""
    ))
    val dumped: String = render(xml)
    assert(xml.gather(el => Option.when(TeiHand.tip.isRef(el))(el)).size == 1, dumped)
    assert(xml.gather(el => Option.when(TeiUnclear.tip.isRef(el))(el)).isEmpty, dumped)
    assert(!dumped.contains("data-cert"), dumped)
    assert(!dumped.contains("data-unclear-tip"), dumped)
    val unclear: Xml.Element = xml.gather(el => Option.when(el.isNamed("unclear"))(el)).head
    assert(unclear.getText == "word", dumped)
    assert(unclear.get("data-medium").contains("pencil"), dumped)
  }

  test("a name link split by pb keeps both halves and the facsimile anchor between them") {
    val xml: Xml.Element = process(
      """<l><orgName ref="Литовское_губернское_правление">Губернское<lb/> <pb n="161-2"/> правленіе</orgName></l>"""
    )
    val dumped: String = render(xml)
    val line: Xml.Element = xml.gather(el => Option.when(el.isNamed("l"))(el)).head
    val names: Seq[Xml.Element] = line.childElements.filter(el => el.isA && el.hasClass("orgName"))
    val pages: Seq[Xml.Element] = line.childElements.filter(Pb.is)
    assert(names.size == 2, dumped)
    assert(pages.size == 1, dumped)
    assert(names.forall(_.getHref.contains("Литовское_губернское_правление")), dumped)
    assert(pages.head.getId.contains("p161-2"), dumped)
    assert(line.childElements.indexOf(names.head) < line.childElements.indexOf(pages.head), dumped)
    assert(line.childElements.indexOf(pages.head) < line.childElements.indexOf(names(1)), dumped)
    assert(names.head.childElements.exists(_.isNamed("lb")), dumped)
    assert(names(1).getText.contains("правленіе"), dumped)
    assert(xml.gather(el => Option.when(el.isA && hasDescendantA(el))(el)).isEmpty, dumped)
  }

  test("a pb inside hi splits the name and keeps xml:id on the first half") {
    val xml: Xml.Element = process(
      """<p><persName xml:id="n1" ref="X">За<hi rend="superscript">л<pb n="1-2"/>ман</hi>ъ</persName></p>"""
    )
    val dumped: String = render(xml)
    val names: Seq[Xml.Element] = xml.gather(el =>
      Option.when(el.isA && el.hasClass("persName"))(el)
    ).toSeq
    assert(names.size == 2, dumped)
    assert(names.forall(name => name.hasClass("persName") && name.getHref.contains("X")), dumped)
    assert(names.forall(name => name.gather(el => Option.when(Pb.is(el))(el)).isEmpty), dumped)
    val firstHi: Xml.Element = names.head.childElements.find(_.isNamed("hi")).get
    assert(names.head.getText.contains("За"), dumped)
    assert(firstHi.get("rend").contains("superscript"), dumped)
    assert(firstHi.getText == "л", dumped)
    assert(names.head.get("xml:id").contains("n1"), dumped)
    val secondHi: Xml.Element = names(1).childElements.find(_.isNamed("hi")).get
    assert(secondHi.get("rend").contains("superscript"), dumped)
    assert(secondHi.getText == "ман", dumped)
    assert(names(1).getText.contains("ъ"), dumped)
    assert(names(1).get("xml:id").isEmpty, dumped)
    assert(names(1).getId.isEmpty, dumped)
    val page: Xml.Element = xml.gather(el => Option.when(Pb.is(el))(el)).head
    assert(page.getId.contains("p1-2"), dumped)
    assert(xml.gather(el => Option.when(el.isA && hasDescendantA(el))(el)).isEmpty, dumped)
  }

  test("a name without ref keeps its pb, and a ref splits around one") {
    val xml: Xml.Element = process(
      """<p><persName>За<pb n="3-1"/>лманъ</persName> <ref target="#somewhere">ab<pb n="3-2"/>cd</ref></p>"""
    )
    val dumped: String = render(xml)
    val persons: Seq[Xml.Element] = xml.gather(el => Option.when(el.isNamed("persName"))(el)).toSeq
    assert(persons.size == 1, dumped)
    assert(persons.head.gather(el => Option.when(Pb.is(el))(el)).nonEmpty, dumped)
    val refs: Seq[Xml.Element] = xml.gather(el => Option.when(el.isA && el.hasClass("ref"))(el)).toSeq
    assert(refs.size == 2, dumped)
    assert(refs.forall(_.getHref.contains("#somewhere")), dumped)
    assert(refs.forall(_.getText.nonEmpty), dumped)
    val page: Xml.Element = xml.gather(el => Option.when(el.isA && el.getId.contains("p3-2"))(el)).head
    assert(Pb.is(page), dumped)
    assert(refs.forall(ref => ref.gather(el => Option.when(Pb.is(el))(el)).isEmpty), dumped)
  }

  test("a page-number fw after pb stays inside the second name half") {
    val xml: Xml.Element = process(
      """<p><persName ref="Нохим_Ицкович">Нохиму<lb/><pb n="107-2"/><fw type="pageNum">107 об.</fw>Ицковичу</persName></p>"""
    )
    val dumped: String = render(xml)
    val names: Seq[Xml.Element] = xml.childElements.filter(el => el.isA && el.hasClass("persName"))
    val pages: Seq[Xml.Element] = xml.childElements.filter(Pb.is)
    assert(names.size == 2, dumped)
    assert(pages.size == 1, dumped)
    assert(xml.childElements.indexOf(pages.head) == xml.childElements.indexOf(names.head) + 1, dumped)
    assert(xml.childElements.indexOf(names(1)) == xml.childElements.indexOf(pages.head) + 1, dumped)
    assert(names.head.childElements.exists(_.isNamed("lb")), dumped)
    assert(!names.head.childElements.exists(_.isNamed("fw")), dumped)
    val fw: Xml.Element = names(1).childElements.find(_.isNamed("fw")).get
    assert(fw.get("type").contains("pageNum"), dumped)
    assert(fw.getText.contains("107 об."), dumped)
    assert(names(1).getText.contains("Ицковичу"), dumped)
    assert(names.forall(_.getHref.contains("Нохим_Ицкович")), dumped)
  }

  private def hasDescendantA(element: Xml.Element): Boolean =
    element.childElements.exists(child => child.isA || hasDescendantA(child))
