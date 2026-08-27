package org.podval.tools.publish.markup

import org.podval.tools.publish.site.PageErrorReporter
import org.podval.xml.{HtmlXmlDialect, Xml, XmlParser}
import org.scalatest.funsuite.AnyFunSuite

final class DocBookMarkupSpec extends AnyFunSuite:
  private def parse(input: String): Xml.Element =
    XmlParser.parseXml(input).toOption.get

  private def processResult(input: String): (Xml.Element, Option[Xml.Element]) =
    DocBookMarkup.process(parse(input), PageErrorReporter.Silent)

  private def process(input: String): Xml.Element =
    processResult(input)._1

  private def render(element: Xml.Element): String =
    HtmlXmlDialect.render(element)

  test("DocBook roots disambiguate .xml; TEI roots stay TEI") {
    val docbook: Set[String] = Set(
      "article", "book", "chapter", "appendix", "part", "set", "preface", "refentry", "topic"
    )
    docbook.foreach: name =>
      assert(Markup.forElement(name).contains(DocBookMarkup), name)
    assert(Markup.forElement("section").isEmpty)
    assert(Markup.forElement("TEI").contains(TeiMarkup))
    assert(Markup.forElement("store").contains(TeiMarkup))
  }

  test("para and simpara become p; document title is extracted") {
    val (xml, title) = processResult(
      """<article>
        |  <title>Doc title</title>
        |  <para>Hello</para>
        |  <simpara>Short</simpara>
        |</article>""".stripMargin
    )
    val dumped: String = render(xml)
    assert(title.exists(_.getName == "db-title"), dumped)
    assert(title.exists(_.getText.contains("Doc title")), dumped)
    assert(!dumped.contains("Doc title"), dumped)
    assert(dumped.contains("<p"), dumped)
    assert(dumped.contains("Hello"), dumped)
    assert(dumped.contains("Short"), dumped)
    assert(!dumped.contains("<para"), dumped)
    assert(!dumped.contains("<simpara"), dumped)
    assert(!dumped.contains("<title"), dumped)
    assert(xml.getName == "article", dumped)
  }

  test("lists become ul/ol/li; emphasis becomes em; quote becomes q") {
    val xml: Xml.Element = process(
      """<article>
        |  <para><emphasis>em</emphasis> and <quote>q</quote>
        |  <subscript>1</subscript><superscript>2</superscript></para>
        |  <itemizedlist><listitem><para>a</para></listitem></itemizedlist>
        |  <orderedlist><listitem><para>b</para></listitem></orderedlist>
        |</article>""".stripMargin
    )
    val dumped: String = render(xml)
    assert(dumped.contains("<em"), dumped)
    assert(dumped.contains("<q"), dumped)
    assert(dumped.contains("<sub"), dumped)
    assert(dumped.contains("<sup"), dumped)
    assert(dumped.contains("<ul"), dumped)
    assert(dumped.contains("<ol"), dumped)
    assert(dumped.contains("<li"), dumped)
    assert(!dumped.contains("<emphasis"), dumped)
    assert(!dumped.contains("<itemizedlist"), dumped)
    assert(!dumped.contains("<orderedlist"), dumped)
    assert(!dumped.contains("<listitem"), dumped)
    assert(!dumped.contains("<quote"), dumped)
  }

  test("row/entry become tr/td; tgroup is unwrapped; colspec dropped") {
    val xml: Xml.Element = process(
      """<table>
        |  <title>Grid</title>
        |  <tgroup cols="2">
        |    <colspec colname="c1"/>
        |    <thead><row><entry>A</entry><entry>B</entry></row></thead>
        |    <tbody><row><entry>1</entry><entry>2</entry></row></tbody>
        |  </tgroup>
        |</table>""".stripMargin
    )
    val dumped: String = render(xml)
    assert(dumped.contains("<tr"), dumped)
    assert(dumped.contains("<td"), dumped)
    assert(dumped.contains("<table"), dumped)
    assert(!dumped.contains("<tgroup"), dumped)
    assert(!dumped.contains("<colspec"), dumped)
    assert(!dumped.contains("<row"), dumped)
    assert(!dumped.contains("<entry"), dumped)
    val cells: Seq[String] = xml.gather(element =>
      Option.when(element.getName == "td")(element.getText.trim)
    ).toSeq.filter(_.nonEmpty)
    assert(cells.contains("A"), dumped)
    assert(cells.contains("B"), dumped)
    assert(cells.contains("1"), dumped)
    assert(cells.contains("2"), dumped)
    assert(xml.gather(el => Option.when(el.getName == "table")(el)).nonEmpty, dumped)
  }

  test("informaltable becomes table; imagedata becomes img; links become a") {
    val xml: Xml.Element = process(
      """<article>
        |  <informaltable><tgroup cols="1"><row><entry>x</entry></row></tgroup></informaltable>
        |  <para><imagedata fileref="pixel.svg"/>
        |  <link linkend="foo">text</link>
        |  <ulink url="https://example.com">ex</ulink>
        |  <xref linkend="foo"/></para>
        |</article>""".stripMargin
    )
    val dumped: String = render(xml)
    assert(dumped.contains("<table"), dumped)
    assert(!dumped.contains("<informaltable"), dumped)
    assert(dumped.contains("<img"), dumped)
    assert(!dumped.contains("<imagedata"), dumped)
    assert(dumped.contains("<a"), dumped)
    assert(!dumped.contains("<link"), dumped)
    assert(!dumped.contains("<ulink"), dumped)
    assert(!dumped.contains("<xref"), dumped)
  }

  test("section and sect1 become div; reserved class is prefixed") {
    val xml: Xml.Element = process(
      """<article>
        |  <section><title>S</title><para>p</para></section>
        |  <sect1 class="keep"><title>One</title><para>q</para></sect1>
        |</article>""".stripMargin
    )
    val dumped: String = render(xml)
    assert(!dumped.contains("<section"), dumped)
    assert(!dumped.contains("<sect1"), dumped)
    val divs: Seq[Xml.Element] = xml.gather(el => Option.when(el.getName == "div")(el)).toSeq
    assert(divs.exists(_.hasClass("section")), dumped)
    val sect1: Xml.Element = divs.find(_.hasClass("sect1")).get
    assert(sect1.get("db-class").contains("keep"), dumped)
    assert(!sect1.hasClass("keep"), dumped)
    assert(divs.forall(Section.is), dumped)
    val headers: Seq[String] = divs.flatMap(div => Section.heading(div).map(_.getText.trim))
    assert(headers.contains("S"), dumped)
    assert(headers.contains("One"), dumped)
  }

  test("info title is the document title; empty info is dropped") {
    val (xml, title) = processResult(
      """<article>
        |  <info><title>From info</title></info>
        |  <para>Hello</para>
        |</article>""".stripMargin
    )
    val dumped: String = render(xml)
    assert(title.exists(_.getText.contains("From info")), dumped)
    assert(!dumped.contains("From info"), dumped)
    assert(xml.gather(el => Option.when(el.getName == "info")(el)).isEmpty, dumped)
  }

  test("root chapter is not renamed to div; nested section is marked") {
    val (xml, title) = processResult(
      """<chapter>
        |  <title>Chapter</title>
        |  <section><title>Nested</title><para>p</para></section>
        |</chapter>""".stripMargin
    )
    val dumped: String = render(xml)
    assert(xml.getName == "chapter", dumped)
    assert(title.exists(_.getText.contains("Chapter")), dumped)
    val sections: Seq[Xml.Element] = xml.gather(el => Option.when(Section.is(el))(el)).toSeq
    assert(sections.size == 1, dumped)
    assert(Section.heading(sections.head).exists(_.getText.trim == "Nested"), dumped)
  }

  test("fileref becomes src; linkend/url/xlink:href become href; empty xref is labeled") {
    val xml: Xml.Element = process(
      """<article xmlns:xlink="http://www.w3.org/1999/xlink">
        |  <para>
        |    <imagedata fileref="pixel.svg"/>
        |    <videodata fileref="clip.mp4"/>
        |    <link linkend="foo">text</link>
        |    <link linkend="#bar">hash</link>
        |    <link xlink:href="https://example.com">ex</link>
        |    <ulink url="https://example.org">org</ulink>
        |    <xref linkend="foo"/>
        |    <xref linkend="z" xreflabel="See Z"/>
        |  </para>
        |</article>""".stripMargin
    )
    val dumped: String = render(xml)
    val img: Xml.Element = xml.gather(el => Option.when(el.getName == "img")(el)).head
    assert(img.get("src").contains("pixel.svg"), dumped)
    val video: Xml.Element = xml.gather(el => Option.when(Video.is(el))(el)).head
    assert(video.get("src").contains("clip.mp4"), dumped)
    val hrefs: Set[String] = xml.gather(el => Option.when(el.isA)(el.getHref)).flatten.toSet
    assert(hrefs.contains("#foo"), dumped)
    assert(hrefs.contains("#bar"), dumped)
    assert(hrefs.contains("https://example.com"), dumped)
    assert(hrefs.contains("https://example.org"), dumped)
    val xrefFoo: Xml.Element = xml.gather(el =>
      Option.when(el.isA && el.getHref.contains("#foo") && el.getText.trim == "foo")(el)
    ).head
    assert(xrefFoo.getText.trim == "foo", dumped)
    val xrefZ: Xml.Element = xml.gather(el =>
      Option.when(el.isA && el.getHref.contains("#z"))(el)
    ).head
    assert(xrefZ.getText.trim == "See Z", dumped)
    val withText: Xml.Element = xml.gather(el =>
      Option.when(el.isA && el.getHref.contains("#foo") && el.getText.trim == "text")(el)
    ).head
    assert(withText.getText.trim == "text", dumped)
  }

  test("entry morerows becomes rowspan n+1") {
    val xml: Xml.Element = process(
      """<table><tgroup cols="1"><row><entry morerows="1">A</entry></row></tgroup></table>"""
    )
    val dumped: String = render(xml)
    val td: Xml.Element = xml.gather(el => Option.when(el.getName == "td")(el)).head
    assert(td.get("rowspan").contains("2"), dumped)
    assert(!dumped.contains("<entry"), dumped)
  }

  test("footnote becomes footnote IR; class is not db-class; footnoteref reuses id") {
    val xml: Xml.Element = process(
      """<article>
        |<para>See this<footnote xml:id="fn1"><para>A note.</para></footnote>
        |and again<footnoteref linkend="fn1"/>.</para>
        |</article>""".stripMargin
    )
    val dumped: String = render(xml)
    assert(dumped.contains("""class="footnote-link""""), dumped)
    assert(!dumped.contains("""db-class="footnote-link""""), dumped)
    assert(dumped.contains("""class="footnote""""), dumped)
    assert(dumped.contains("A note"), dumped)
    val ids: Seq[String] = Footnote.linkIds(xml).toSeq
    assert(ids == Seq("fn1", "fn1"), dumped)
    val leftover: Seq[Xml.Element] = xml.gather(el => Option.when(el.getName == "footnote")(el)).toSeq
    assert(leftover.isEmpty, dumped)
  }

  test("footnote after text or a preceding element has no separating HTML space") {
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

    clings(published("""<para>See this<footnote><para>A note.</para></footnote>.</para>"""), "this")
    clings(published("""<para>See <emphasis>this</emphasis><footnote><para>A note.</para></footnote>.</para>"""), "</em>")
  }

  test("glosslist becomes glossary IR; variablelist is a plain dl") {
    val xml: Xml.Element = process(
      """<article>
        |<para>See <link linkend="posuk">posuk</link>.</para>
        |<glosslist>
        |  <glossentry xml:id="posuk"><glossterm>posuk</glossterm><glossdef><para>verse</para></glossdef></glossentry>
        |  <glossentry><glossterm>mud</glossterm><glossdef><para>wet dirt</para></glossdef></glossentry>
        |</glosslist>
        |<variablelist>
        |  <varlistentry><term>alpha</term><listitem><para>first</para></listitem></varlistentry>
        |</variablelist>
        |</article>""".stripMargin
    )
    val dumped: String = render(xml)
    assert(dumped.contains("glossary-item"), dumped)
    assert(!dumped.contains("""db-class="glossary""""), dumped)
    val defs: Map[String, Xml.Nodes] = Glossary.definitions(xml)
    assert(defs.keySet == Set("posuk", "mud"), dumped)
    assert(Xml.toString(defs("posuk")).contains("verse"), dumped)
    assert(xml.gather(el => Option.when(Glossary.isList(el))(el)).size == 1, dumped)
    val dls: Seq[Xml.Element] = xml.gather(el => Option.when(el.getName == "dl")(el)).toSeq
    assert(dls.size == 2, dumped)
    assert(dls.exists(dl => !Glossary.isList(dl) && dl.getText.contains("alpha")), dumped)
  }

  test("programlisting and code language; literal is inline code") {
    val xml: Xml.Element = process(
      """<article>
        |<para>Use <code language="scala">xs.map(f)</code> and <literal>x</literal></para>
        |<programlisting language="JAVA">Size s = new Size();
        |s.Width = 500;</programlisting>
        |</article>""".stripMargin
    )
    val dumped: String = render(xml)
    assert(dumped.contains("""class="language-scala""""), dumped)
    assert(dumped.contains("""class="language-java""""), dumped)
    assert(dumped.contains("<pre"), dumped)
    assert(xml.gather(el => Option.when(el.getName == "literal")(el)).isEmpty, dumped)
  }

  test("blockquote with attribution becomes quote IR; inline quote stays q") {
    val xml: Xml.Element = process(
      """<article>
        |<para>See <quote>inline</quote>.</para>
        |<blockquote>
        |  <title>A title</title>
        |  <para>A DocBook quotation.</para>
        |  <attribution>Jefferson</attribution>
        |</blockquote>
        |</article>""".stripMargin
    )
    val dumped: String = render(xml)
    assert(xml.gather(el => Option.when(Quote.is(el))(el)).size == 1, dumped)
    assert(dumped.contains("A DocBook quotation"), dumped)
    assert(dumped.contains("Jefferson"), dumped)
    assert(dumped.contains("A title"), dumped)
    assert(dumped.contains("<q"), dumped)
    assert(dumped.contains("inline"), dumped)
  }

  test("figure with imagedata becomes figure IR") {
    val xml: Xml.Element = process(
      """<article>
        |<figure xml:id="fig1">
        |  <title>A DocBook figure</title>
        |  <mediaobject><imageobject><imagedata fileref="pixel.svg"/></imageobject></mediaobject>
        |</figure>
        |</article>""".stripMargin
    )
    val dumped: String = render(xml)
    assert(xml.gather(el => Option.when(Figure.is(el))(el)).size == 1, dumped)
    assert(dumped.contains("pixel.svg"), dumped)
    assert(dumped.contains("A DocBook figure"), dumped)
    assert(!dumped.contains("<imagedata"), dumped)
    assert(!dumped.contains("<mediaobject"), dumped)
  }

  test("emphasis roles: bold, strikethrough; default em") {
    val xml: Xml.Element = process(
      """<para><emphasis>em</emphasis> <emphasis role="bold">b</emphasis>
        |<emphasis role="strikethrough">old</emphasis></para>""".stripMargin
    )
    val dumped: String = render(xml)
    assert(dumped.contains("<em"), dumped)
    assert(dumped.contains("<strong"), dumped)
    assert(dumped.contains("<del"), dumped)
    assert(dumped.contains("old"), dumped)
  }

  test("note/tip become admonitions; sidebar becomes aside") {
    val xml: Xml.Element = process(
      """<article>
        |<note><title>Save time</title><para>Use the shortcut.</para></note>
        |<tip><para>A tip.</para></tip>
        |<sidebar><title>Optional Title</title><para>Auxiliary content.</para></sidebar>
        |</article>""".stripMargin
    )
    val dumped: String = render(xml)
    val admonitions: Seq[Xml.Element] = xml.gather(el => Option.when(Admonition.is(el))(el)).toSeq
    assert(admonitions.map(_.get(Admonition.TypeAttr)).toSet == Set(Some("note"), Some("tip")), dumped)
    assert(dumped.contains("Save time"), dumped)
    assert(xml.gather(el => Option.when(Aside.is(el))(el)).size == 1, dumped)
    assert(dumped.contains("Auxiliary content"), dumped)
  }

  test("co and calloutlist become callout IR") {
    val xml: Xml.Element = process(
      """<article>
        |<programlisting>require 'sinatra'<co xml:id="co1" label="1"/></programlisting>
        |<calloutlist><callout arearefs="co1"><para>Library import</para></callout></calloutlist>
        |</article>""".stripMargin
    )
    val dumped: String = render(xml)
    assert(xml.gather(el => Option.when(Callout.isMark(el))(el), stopAtCode = false).size == 1, dumped)
    assert(xml.gather(el => Option.when(Callout.isList(el))(el)).size == 1, dumped)
    assert(dumped.contains("Library import"), dumped)
    assert(dumped.contains("""data-value="1""""), dumped)
  }

  test("videodata becomes video IR") {
    val xml: Xml.Element = process(
      """<article><mediaobject><videoobject><videodata fileref="clip.mp4"/></videoobject></mediaobject></article>"""
    )
    val dumped: String = render(xml)
    assert(xml.gather(el => Option.when(Video.is(el))(el)).size == 1, dumped)
    assert(dumped.contains("clip.mp4"), dumped)
    assert(!dumped.contains("<videodata"), dumped)
  }

  test("bibliography entries get id; empty bibliography is citeproc placeholder") {
    val xml: Xml.Element = process(
      """<article>
        |<para>See <link linkend="knuth79">Knuth 1979</link>
        |and <biblioref linkend="lamport94"/>
        |and <citation>knuth79, p. 12</citation>.</para>
        |<bibliography>
        |  <biblioentry xml:id="knuth79">Knuth, Donald E.</biblioentry>
        |</bibliography>
        |<bibliography/>
        |</article>""".stripMargin
    )
    val dumped: String = render(xml)
    val items: Seq[Xml.Element] = xml.gather(el => Option.when(BibliographyItem.isItem(el))(el)).toSeq
    assert(items.map(_.getId).toSet == Set(Some("knuth79")), dumped)
    assert(dumped.contains("""href="#knuth79""""), dumped)
    val stubs: Seq[Xml.Element] = xml.gather(el => Option.when(Citation.isCite(el))(el)).toSeq
    val stubItems: Seq[Citation.Item] = stubs.flatMap(Citation.itemsOf)
    assert(stubItems.map(_.key).toSet == Set("lamport94", "knuth79"), dumped)
    assert(stubItems.exists(item => item.key == "knuth79" && item.locator.contains("p. 12")), dumped)
    assert(xml.gather(el => Option.when(Citation.isPlaceholder(el))(el)).size == 1, dumped)
  }
