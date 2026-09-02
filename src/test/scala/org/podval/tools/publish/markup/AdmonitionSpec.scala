package org.podval.tools.publish.markup

import org.asciidoctor.Asciidoctor
import org.podval.tools.publish.site.PageErrorReporter
import org.podval.xml.{HtmlXmlWriterConfig, Xml, XmlParser}
import org.scalatest.funsuite.AnyFunSuite
import java.io.File

final class AdmonitionSpec extends AnyFunSuite:
  private lazy val asciidoctor: Asciidoctor =
    val result: Asciidoctor = Asciidoctor.Factory.create()
    AsciiDocCiteExtension.register(result)
    result

  private def render(element: Xml.Element): String = HtmlXmlWriterConfig.render(element)

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

  private def fromDocBook(source: String): Xml.Element =
    DocBookMarkup.process(parse(source), PageErrorReporter.Silent)._1

  private def admonitions(xml: Xml.Element): Seq[Xml.Element] =
    xml.gather(element => Option.when(Admonition.is(element))(element)).toSeq

  test("AsciiDoc NOTE paragraph becomes admonition IR") {
    val xml: Xml.Element = fromAsciiDoc("NOTE: Auxiliary information.\n")
    val dumped: String = render(xml)
    val found: Seq[Xml.Element] = admonitions(xml)
    assert(found.size == 1, dumped)
    assert(found.head.get(Admonition.TypeAttr).contains("note"), dumped)
    assert(found.head.getName == "div", dumped)
    assert(dumped.contains("""class="admonition-title""""), dumped)
    assert(dumped.contains("Auxiliary information"), dumped)
    assert(!dumped.contains("admonitionblock"), dumped)
  }

  test("AsciiDoc WARNING block with title") {
    val xml: Xml.Element = fromAsciiDoc(
      """.Watch out
        |[WARNING]
        |====
        |Multi-paragraph body.
        |====
        |""".stripMargin
    )
    val dumped: String = render(xml)
    val found: Seq[Xml.Element] = admonitions(xml)
    assert(found.size == 1, dumped)
    assert(found.head.get(Admonition.TypeAttr).contains("warning"), dumped)
    assert(dumped.contains("Watch out"), dumped)
    assert(dumped.contains("Multi-paragraph body"), dumped)
  }

  test("Markdown Obsidian [!tip] with title becomes admonition IR") {
    val xml: Xml.Element = fromMarkdown(
      """> [!tip] Save time
        |> Use the shortcut.
        |""".stripMargin
    )
    val dumped: String = render(xml)
    val found: Seq[Xml.Element] = admonitions(xml)
    assert(found.size == 1, dumped)
    assert(found.head.get(Admonition.TypeAttr).contains("tip"), dumped)
    assert(found.head.getName == "div", dumped)
    assert(dumped.contains("Save time"), dumped)
    assert(dumped.contains("Use the shortcut"), dumped)
    assert(!dumped.contains("<blockquote"), dumped)
  }

  test("Markdown [!faq]- fold is a closed details") {
    val xml: Xml.Element = fromMarkdown(
      """> [!faq]- Hidden
        |> Secret.
        |""".stripMargin
    )
    val dumped: String = render(xml)
    val found: Seq[Xml.Element] = admonitions(xml)
    assert(found.size == 1, dumped)
    assert(found.head.getName == "details", dumped)
    assert(found.head.get("open").isEmpty, dumped)
    assert(found.head.get(Admonition.TypeAttr).contains("faq"), dumped)
    assert(dumped.contains("<summary"), dumped)
    assert(dumped.contains("Hidden"), dumped)
    assert(dumped.contains("Secret"), dumped)
  }

  test("Markdown [!note]+ fold is an open details") {
    val xml: Xml.Element = fromMarkdown(
      """> [!note]+ Shown
        |> Visible.
        |""".stripMargin
    )
    val dumped: String = render(xml)
    assert(admonitions(xml).head.get("open").contains("open"), dumped)
  }

  test("HTML that is already IR is unchanged by HtmlMarkup.process") {
    val ir: Xml.Element = parse(
      """<div><div class="admonition" data-type="note"><div class="admonition-title">Note</div>
        |<p>already</p></div></div>"""
    )
    val processed: Xml.Element = HtmlMarkup.process(ir, PageErrorReporter.Silent)._1
    val dumped: String = render(processed)
    assert(admonitions(processed).size == 1, dumped)
    assert(dumped.contains("""data-type="note""""), dumped)
    assert(dumped.contains("already"), dumped)
  }

  test("plain blockquote is not an admonition") {
    val xml: Xml.Element = fromMarkdown("> just a quote\n")
    val dumped: String = render(xml)
    assert(admonitions(xml).isEmpty, dumped)
    assert(dumped.contains("<blockquote"), dumped)
  }

  test("Markdown [!note] without a title uses Note") {
    val xml: Xml.Element = fromMarkdown(
      """> [!note]
        |> just body
        |""".stripMargin
    )
    val dumped: String = render(xml)
    val found: Seq[Xml.Element] = admonitions(xml)
    assert(found.size == 1, dumped)
    assert(found.head.get(Admonition.TypeAttr).contains("note"), dumped)
    val titles: Seq[String] = found.head.gather(element =>
      Option.when(Admonition.isTitle(element))(element.getText.trim)
    ).toSeq
    assert(titles == Seq("Note"), dumped)
    assert(dumped.contains("just body"), dumped)
  }

  test("AsciiDoc TIP IMPORTANT CAUTION become admonition IR") {
    val xml: Xml.Element = fromAsciiDoc(
      """TIP: A hint.
        |
        |IMPORTANT: Do this.
        |
        |CAUTION: Slow down.
        |""".stripMargin
    )
    val dumped: String = render(xml)
    val found: Seq[Xml.Element] = admonitions(xml)
    assert(found.map(_.get(Admonition.TypeAttr)) == Seq(Some("tip"), Some("important"), Some("caution")), dumped)
    assert(dumped.contains("A hint"), dumped)
    assert(dumped.contains("Do this"), dumped)
    assert(dumped.contains("Slow down"), dumped)
    assert(!dumped.contains("admonitionblock"), dumped)
  }

  test("nested Obsidian callout converts inner and outer") {
    val xml: Xml.Element = fromMarkdown(
      """> [!question] Outer
        |> > [!tip] Inner
        |> > Nested.
        |""".stripMargin
    )
    val dumped: String = render(xml)
    val found: Seq[Xml.Element] = admonitions(xml)
    assert(found.size == 2, dumped)
    assert(found.map(_.get(Admonition.TypeAttr)).toSet == Set(Some("question"), Some("tip")), dumped)
    val outer: Xml.Element = found.find(_.get(Admonition.TypeAttr).contains("question")).get
    val inner: Xml.Element = found.find(_.get(Admonition.TypeAttr).contains("tip")).get
    assert(outer.gather(element => Option.when(element eq inner)(element)).nonEmpty, dumped)
    assert(dumped.contains("Outer"), dumped)
    assert(dumped.contains("Inner"), dumped)
    assert(dumped.contains("Nested"), dumped)
    assert(!dumped.contains("<blockquote"), dumped)
  }

  test("DocBook note with title becomes admonition IR") {
    val xml: Xml.Element = fromDocBook(
      """<article>
        |<note><title>Save time</title><para>Use the shortcut.</para></note>
        |<tip><para>A tip.</para></tip>
        |</article>""".stripMargin
    )
    val dumped: String = render(xml)
    val found: Seq[Xml.Element] = admonitions(xml)
    assert(found.map(_.get(Admonition.TypeAttr)).toSet == Set(Some("note"), Some("tip")), dumped)
    assert(dumped.contains("Save time"), dumped)
    assert(dumped.contains("Use the shortcut"), dumped)
    assert(dumped.contains("A tip"), dumped)
    assert(!dumped.contains("db-class"), dumped)
  }
