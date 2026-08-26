package org.podval.tools.publish.markup

import org.asciidoctor.Asciidoctor
import org.podval.tools.publish.site.PageErrorReporter
import org.podval.xml.{HtmlXmlDialect, Xml, XmlParser}
import org.scalatest.funsuite.AnyFunSuite
import java.io.File

final class AsideSpec extends AnyFunSuite:
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

  private def fromDocBook(source: String): Xml.Element =
    DocBookMarkup.process(parse(source), PageErrorReporter.Silent)._1

  private def asides(xml: Xml.Element): Seq[Xml.Element] =
    xml.gather(element => Option.when(Aside.is(element))(element)).toSeq

  test("AsciiDoc **** sidebar with title becomes aside IR") {
    val xml: Xml.Element = fromAsciiDoc(
      """.Optional Title
        |****
        |Auxiliary content.
        |****
        |""".stripMargin
    )
    val dumped: String = render(xml)
    val found: Seq[Xml.Element] = asides(xml)
    assert(found.size == 1, dumped)
    assert(found.head.getName == "aside", dumped)
    assert(dumped.contains("""class="aside-title""""), dumped)
    assert(dumped.contains("Optional Title"), dumped)
    assert(dumped.contains("Auxiliary content"), dumped)
    assert(!dumped.contains("sidebarblock"), dumped)
  }

  test("AsciiDoc [sidebar] paragraph becomes aside IR without title") {
    val xml: Xml.Element = fromAsciiDoc(
      """[sidebar]
        |Sidebars supplement the main text.
        |""".stripMargin
    )
    val dumped: String = render(xml)
    val found: Seq[Xml.Element] = asides(xml)
    assert(found.size == 1, dumped)
    assert(asides(xml).flatMap(el => el.gather(e => Option.when(Aside.isTitle(e))(e))).isEmpty, dumped)
    assert(dumped.contains("Sidebars supplement"), dumped)
    assert(!dumped.contains("sidebarblock"), dumped)
  }

  test("HTML <aside> without class gets class aside") {
    val xml: Xml.Element = HtmlMarkup.process(
      parse("""<div><aside><p>raw aside</p></aside></div>"""),
      PageErrorReporter.Silent
    )._1
    val dumped: String = render(xml)
    assert(asides(xml).size == 1, dumped)
    assert(dumped.contains("""class="aside""""), dumped)
    assert(dumped.contains("raw aside"), dumped)
  }

  test("HTML that is already IR is unchanged by HtmlMarkup.process") {
    val ir: Xml.Element = parse(
      """<div><aside class="aside"><div class="aside-title">Title</div>
        |<p>already</p></aside></div>"""
    )
    val processed: Xml.Element = HtmlMarkup.process(ir, PageErrorReporter.Silent)._1
    val dumped: String = render(processed)
    assert(asides(processed).size == 1, dumped)
    assert(dumped.contains("""class="aside-title""""), dumped)
    assert(dumped.contains("already"), dumped)
  }

  test("Markdown HTML aside block gets class aside") {
    val xml: Xml.Element = fromMarkdown(
      """<aside>
        |<p>From Markdown.</p>
        |</aside>
        |""".stripMargin
    )
    val dumped: String = render(xml)
    assert(asides(xml).size == 1, dumped)
    assert(dumped.contains("From Markdown"), dumped)
  }

  test("plain blockquote is not an aside") {
    val xml: Xml.Element = fromMarkdown("> just a quote\n")
    val dumped: String = render(xml)
    assert(asides(xml).isEmpty, dumped)
  }

  test("AsciiDoc sidebar containing TIP keeps the inner admonition") {
    val xml: Xml.Element = fromAsciiDoc(
      """****
        |TIP: A hint inside.
        |****
        |""".stripMargin
    )
    val dumped: String = render(xml)
    val foundAsides: Seq[Xml.Element] = asides(xml)
    assert(foundAsides.size == 1, dumped)
    val inner: Seq[Xml.Element] = foundAsides.head.gather(element =>
      Option.when(Admonition.is(element))(element)
    ).toSeq
    assert(inner.size == 1, dumped)
    assert(inner.head.get(Admonition.TypeAttr).contains("tip"), dumped)
    assert(dumped.contains("A hint inside"), dumped)
    assert(!dumped.contains("sidebarblock"), dumped)
    assert(!dumped.contains("admonitionblock"), dumped)
  }

  test("HTML aside does not promote a leading heading to aside-title") {
    val xml: Xml.Element = HtmlMarkup.process(
      parse("""<div><aside><h2>Not a title</h2><p>body</p></aside></div>"""),
      PageErrorReporter.Silent
    )._1
    val dumped: String = render(xml)
    val found: Seq[Xml.Element] = asides(xml)
    assert(found.size == 1, dumped)
    assert(found.head.gather(element => Option.when(Aside.isTitle(element))(element)).isEmpty, dumped)
    assert(dumped.contains("<h2"), dumped)
    assert(dumped.contains("Not a title"), dumped)
    assert(dumped.contains("body"), dumped)
  }

  test("DocBook sidebar with title becomes aside IR") {
    val xml: Xml.Element = fromDocBook(
      """<article>
        |<sidebar><title>Optional Title</title><para>Auxiliary content.</para></sidebar>
        |</article>""".stripMargin
    )
    val dumped: String = render(xml)
    val found: Seq[Xml.Element] = asides(xml)
    assert(found.size == 1, dumped)
    assert(found.head.getName == "aside", dumped)
    assert(dumped.contains("""class="aside-title""""), dumped)
    assert(dumped.contains("Optional Title"), dumped)
    assert(dumped.contains("Auxiliary content"), dumped)
    assert(!dumped.contains("<sidebar"), dumped)
    assert(!dumped.contains("db-class"), dumped)
  }
