package org.podval.tools.publish.markup

import org.asciidoctor.Asciidoctor
import org.podval.tools.publish.site.PageErrorReporter
import org.podval.xml.{HtmlXmlDialect, Xml, XmlParser}
import org.scalatest.funsuite.AnyFunSuite
import java.io.File

final class QuoteSpec extends AnyFunSuite:
  private lazy val asciidoctor: Asciidoctor =
    val result: Asciidoctor = Asciidoctor.Factory.create()
    AsciiDocCiteExtension.register(result)
    result

  private def render(element: Xml.Element): String = HtmlXmlDialect.render(element)

  private def parse(xml: String): Xml.Element = XmlParser.parseXml(xml).toOption.get

  private def fromAsciiDoc(source: String): Xml.Element =
    AsciiDocMarkup.process(
      parse(AsciiDocMarkup.convert(source, File("t.adoc").getAbsoluteFile, asciidoctor)),
      PageErrorReporter.Silent
    )._1

  private def fromMarkdown(source: String): Xml.Element =
    MarkdownMarkup.process(
      parse(MarkdownMarkup.xmlContent(source, File("t.md"))),
      PageErrorReporter.Silent
    )._1

  private def quotes(xml: Xml.Element): Seq[Xml.Element] =
    xml.gather(element => Option.when(Quote.is(element))(element)).toSeq

  test("AsciiDoc [quote] with title and attribution becomes quote IR") {
    val xml: Xml.Element = fromAsciiDoc(
      """.A title
        |[quote, Jefferson, Papers]
        |____
        |A little rebellion now and then is a good thing.
        |____
        |""".stripMargin
    )
    val dumped: String = render(xml)
    val found: Seq[Xml.Element] = quotes(xml)
    assert(found.size == 1, dumped)
    assert(found.head.getName == "blockquote", dumped)
    assert(dumped.contains("""class="quote-title""""), dumped)
    assert(dumped.contains("A title"), dumped)
    assert(dumped.contains("A little rebellion"), dumped)
    assert(dumped.contains("""class="quote-attribution""""), dumped)
    assert(dumped.contains("Jefferson"), dumped)
    assert(dumped.contains("<cite"), dumped)
    assert(dumped.contains("Papers"), dumped)
    assert(!dumped.contains("quoteblock"), dumped)
  }

  test("AsciiDoc [quote] block without attribution has no footer") {
    val xml: Xml.Element = fromAsciiDoc(
      """[quote]
        |____
        |Bare quotation.
        |____
        |""".stripMargin
    )
    val dumped: String = render(xml)
    val found: Seq[Xml.Element] = quotes(xml)
    assert(found.size == 1, dumped)
    assert(found.head.gather(element => Option.when(Quote.isTitle(element))(element)).isEmpty, dumped)
    assert(found.head.gather(element => Option.when(Quote.isAttribution(element))(element)).isEmpty, dumped)
    assert(dumped.contains("Bare quotation"), dumped)
    assert(!dumped.contains("quoteblock"), dumped)
    assert(!dumped.contains("<footer"), dumped)
  }

  test("AsciiDoc quote keeps block id") {
    val xml: Xml.Element = fromAsciiDoc(
      """[#quoted]
        |[quote]
        |____
        |Identified quotation.
        |____
        |""".stripMargin
    )
    val dumped: String = render(xml)
    val found: Seq[Xml.Element] = quotes(xml)
    assert(found.size == 1, dumped)
    assert(found.head.getId.contains("quoted"), dumped)
    assert(dumped.contains("Identified quotation"), dumped)
  }

  test("Markdown blockquote becomes quote IR") {
    val xml: Xml.Element = fromMarkdown("> A Markdown quotation.\n")
    val dumped: String = render(xml)
    val found: Seq[Xml.Element] = quotes(xml)
    assert(found.size == 1, dumped)
    assert(dumped.contains("A Markdown quotation"), dumped)
    assert(found.head.gather(element => Option.when(Quote.isAttribution(element))(element)).isEmpty, dumped)
  }

  test("Markdown does not invent attribution from an em-dash line") {
    val xml: Xml.Element = fromMarkdown(
      """> quoted text
        |>
        |> — Author
        |""".stripMargin
    )
    val dumped: String = render(xml)
    val found: Seq[Xml.Element] = quotes(xml)
    assert(found.size == 1, dumped)
    assert(found.head.gather(element => Option.when(Quote.isAttribution(element))(element)).isEmpty, dumped)
    assert(!dumped.contains("<footer"), dumped)
    assert(dumped.contains("Author"), dumped)
  }

  test("Markdown [!tip] stays an admonition, not a quote") {
    val xml: Xml.Element = fromMarkdown(
      """> [!tip] Save time
        |> Use the shortcut.
        |""".stripMargin
    )
    val dumped: String = render(xml)
    assert(quotes(xml).isEmpty, dumped)
    val found: Seq[Xml.Element] = xml.gather(element => Option.when(Admonition.is(element))(element)).toSeq
    assert(found.size == 1, dumped)
    assert(!dumped.contains("<blockquote"), dumped)
  }

  test("HTML <blockquote> without class gets class quote") {
    val xml: Xml.Element = HtmlMarkup.process(
      parse("""<div><blockquote><p>raw quote</p></blockquote></div>"""),
      PageErrorReporter.Silent
    )._1
    val dumped: String = render(xml)
    assert(quotes(xml).size == 1, dumped)
    assert(dumped.contains("""class="quote""""), dumped)
    assert(dumped.contains("raw quote"), dumped)
  }

  test("HTML footer on a blockquote gets class quote-attribution") {
    val xml: Xml.Element = HtmlMarkup.process(
      parse(
        """<div><blockquote><p>quoted already</p>
          |<footer>— Author, <cite>Work</cite></footer></blockquote></div>"""
      ),
      PageErrorReporter.Silent
    )._1
    val dumped: String = render(xml)
    val found: Seq[Xml.Element] = quotes(xml)
    assert(found.size == 1, dumped)
    assert(dumped.contains("""class="quote-attribution""""), dumped)
    assert(dumped.contains("Author"), dumped)
    assert(dumped.contains("Work"), dumped)
  }

  test("HTML that is already IR is unchanged by HtmlMarkup.process") {
    val ir: Xml.Element = parse(
      """<div><blockquote class="quote"><div class="quote-title">Title</div>
        |<p>already</p>
        |<footer class="quote-attribution">— Author</footer></blockquote></div>"""
    )
    val processed: Xml.Element = HtmlMarkup.process(ir, PageErrorReporter.Silent)._1
    val dumped: String = render(processed)
    assert(quotes(processed).size == 1, dumped)
    assert(dumped.contains("""class="quote-title""""), dumped)
    assert(dumped.contains("already"), dumped)
    assert(dumped.contains("""class="quote-attribution""""), dumped)
  }

  test("Markdown HTML blockquote block gets class quote") {
    val xml: Xml.Element = fromMarkdown(
      """<blockquote>
        |<p>From Markdown.</p>
        |</blockquote>
        |""".stripMargin
    )
    val dumped: String = render(xml)
    assert(quotes(xml).size == 1, dumped)
    assert(dumped.contains("From Markdown"), dumped)
  }

  test("AsciiDoc NOTE containing a quote keeps both") {
    val xml: Xml.Element = fromAsciiDoc(
      """[NOTE]
        |====
        |[quote]
        |____
        |inside note
        |____
        |====
        |""".stripMargin
    )
    val dumped: String = render(xml)
    val notes: Seq[Xml.Element] = xml.gather(element => Option.when(Admonition.is(element))(element)).toSeq
    assert(notes.size == 1, dumped)
    val inner: Seq[Xml.Element] = notes.head.gather(element => Option.when(Quote.is(element))(element)).toSeq
    assert(inner.size == 1, dumped)
    assert(dumped.contains("inside note"), dumped)
    assert(!dumped.contains("quoteblock"), dumped)
    assert(!dumped.contains("admonitionblock"), dumped)
  }

  test("AsciiDoc quote containing TIP keeps the inner admonition") {
    val xml: Xml.Element = fromAsciiDoc(
      """[quote]
        |____
        |TIP: A hint inside.
        |____
        |""".stripMargin
    )
    val dumped: String = render(xml)
    val foundQuotes: Seq[Xml.Element] = quotes(xml)
    assert(foundQuotes.size == 1, dumped)
    val inner: Seq[Xml.Element] = foundQuotes.head.gather(element =>
      Option.when(Admonition.is(element))(element)
    ).toSeq
    assert(inner.size == 1, dumped)
    assert(inner.head.get(Admonition.TypeAttr).contains("tip"), dumped)
    assert(dumped.contains("A hint"), dumped)
    assert(dumped.contains("inside"), dumped)
    assert(!dumped.contains("quoteblock"), dumped)
    assert(!dumped.contains("admonitionblock"), dumped)
  }

  test("nested AsciiDoc quotes convert inner and outer") {
    val xml: Xml.Element = fromAsciiDoc(
      """[quote]
        |____
        |outer
        |[quote]
        |____
        |inner
        |____
        |____
        |""".stripMargin
    )
    val dumped: String = render(xml)
    val found: Seq[Xml.Element] = quotes(xml)
    assert(found.size == 2, dumped)
    assert(dumped.contains("outer"), dumped)
    assert(dumped.contains("inner"), dumped)
    assert(!dumped.contains("quoteblock"), dumped)
  }

  private def fromTei(source: String): Xml.Element =
    TeiMarkup.process(parse(source), PageErrorReporter.Silent)._1

  test("TEI quote becomes quote IR") {
    val xml: Xml.Element = fromTei(
      """<div><quote>A TEI quotation.</quote></div>"""
    )
    val dumped: String = render(xml)
    val found: Seq[Xml.Element] = quotes(xml)
    assert(found.size == 1, dumped)
    assert(found.head.getName == "blockquote", dumped)
    assert(dumped.contains("A TEI quotation"), dumped)
    assert(!dumped.contains("<quote"), dumped)
    assert(!dumped.contains("tei-class"), dumped)
  }

  test("TEI cit with bibl becomes quote IR with attribution") {
    val xml: Xml.Element = fromTei(
      """<div>
        |<cit>
        |  <quote>A little rebellion now and then is a good thing.</quote>
        |  <bibl>Jefferson, <title>Papers</title></bibl>
        |</cit>
        |</div>""".stripMargin
    )
    val dumped: String = render(xml)
    val found: Seq[Xml.Element] = quotes(xml)
    assert(found.size == 1, dumped)
    assert(dumped.contains("A little rebellion"), dumped)
    assert(dumped.contains("""class="quote-attribution""""), dumped)
    assert(dumped.contains("<cite"), dumped)
    assert(dumped.contains("Jefferson"), dumped)
    assert(dumped.contains("Papers"), dumped)
    assert(xml.gather(el => Option.when(el.getName == "cit")(el)).isEmpty, dumped)
    assert(!dumped.contains("<quote"), dumped)
    assert(!dumped.contains("<bibl"), dumped)
  }

  test("TEI quote with inner bibl uses it as attribution") {
    val xml: Xml.Element = fromTei(
      """<div>
        |<quote xml:id="quoted">Quoted text.<bibl>Author</bibl></quote>
        |</div>""".stripMargin
    )
    val dumped: String = render(xml)
    val found: Seq[Xml.Element] = quotes(xml)
    assert(found.size == 1, dumped)
    assert(found.head.getId.contains("quoted"), dumped)
    assert(dumped.contains("Quoted text"), dumped)
    assert(dumped.contains("""class="quote-attribution""""), dumped)
    assert(dumped.contains("Author"), dumped)
    assert(!dumped.contains("<quote"), dumped)
  }

  test("TEI cit keeps xml:id on the quote") {
    val xml: Xml.Element = fromTei(
      """<div>
        |<cit xml:id="cited">
        |  <quote>Identified quotation.</quote>
        |  <bibl>Source</bibl>
        |</cit>
        |</div>""".stripMargin
    )
    val dumped: String = render(xml)
    val found: Seq[Xml.Element] = quotes(xml)
    assert(found.size == 1, dumped)
    assert(found.head.getId.contains("cited"), dumped)
    assert(dumped.contains("Identified quotation"), dumped)
  }

  test("TEI q stays inline and is not a quote") {
    val xml: Xml.Element = fromTei(
      """<div><p>He said <q>hello</q>.</p></div>"""
    )
    val dumped: String = render(xml)
    assert(quotes(xml).isEmpty, dumped)
    assert(dumped.contains("<q"), dumped)
    assert(dumped.contains("hello"), dumped)
  }

  test("TEI standalone bibl is not a quote") {
    val xml: Xml.Element = fromTei(
      """<div><p>See <bibl>Jefferson</bibl>.</p></div>"""
    )
    val dumped: String = render(xml)
    assert(quotes(xml).isEmpty, dumped)
    assert(dumped.contains("<bibl"), dumped)
    assert(dumped.contains("Jefferson"), dumped)
  }

  test("nested TEI quotes convert inner and outer") {
    val xml: Xml.Element = fromTei(
      """<div>
        |<quote>outer <quote>inner</quote></quote>
        |</div>""".stripMargin
    )
    val dumped: String = render(xml)
    val found: Seq[Xml.Element] = quotes(xml)
    assert(found.size == 2, dumped)
    assert(dumped.contains("outer"), dumped)
    assert(dumped.contains("inner"), dumped)
    assert(!dumped.contains("<quote"), dumped)
  }
