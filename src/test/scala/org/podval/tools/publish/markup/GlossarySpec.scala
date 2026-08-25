package org.podval.tools.publish.markup

import org.asciidoctor.Asciidoctor
import org.podval.xml.{HtmlXmlDialect, Xml, XmlParser}
import org.scalatest.funsuite.AnyFunSuite
import zio.blocks.chunk.Chunk
import java.io.File

final class GlossarySpec extends AnyFunSuite:
  private lazy val asciidoctor: Asciidoctor = Asciidoctor.Factory.create()

  private def parse(input: String): Xml.Element =
    XmlParser.parseXml(input).toOption.get

  private def fromAsciiDoc(source: String): Xml.Element =
    AsciiDocMarkup.cleanup(
      parse(AsciiDocMarkup.convert(source, File("snippet.adoc").getAbsoluteFile, asciidoctor))
    )

  private def fromMarkdown(source: String): Xml.Element =
    MarkdownMarkup.convert(parse(MarkdownMarkup.xmlContent(source, File("t.md"))))

  private def render(element: Xml.Element): String =
    HtmlXmlDialect.render(element)

  private def definitionText(defs: Map[String, Xml.Nodes], id: String): String =
    Xml.toString(defs(id)).trim

  test("definitions are taken from glossary-item dd, not empty or non-glossary lists") {
    val xml: Xml.Element = parse(
      """<div>
        |<div class="glossary">
        |<dl>
        |<div class="glossary-item" id="posuk"><dt>posuk</dt><dd>verse</dd></div>
        |<div class="glossary-item" id="akdamus"><dt>akdamus</dt></div>
        |<div class="glossary-item" id="rasha"><dt>rasha</dt><dd>sinner</dd></div>
        |</dl>
        |</div>
        |<dl>
        |<dt>CPU</dt><dd>processor</dd>
        |</dl>
        |</div>
        |""".stripMargin
    )
    val defs: Map[String, Xml.Nodes] = Glossary.definitions(xml, HtmlXmlDialect)
    assert(defs.keySet == Set("posuk", "rasha"))
    assert(definitionText(defs, "posuk") == "verse")
    assert(definitionText(defs, "rasha") == "sinner")
  }

  test("definitions see dd wrapped in a dl inside the glossary-item") {
    val xml: Xml.Element = parse(
      """<div class="glossary-item" id="html-term"><dl><dt>html-term</dt><dd>defined in HTML</dd></dl></div>"""
    )
    val defs: Map[String, Xml.Nodes] = Glossary.definitions(xml, HtmlXmlDialect)
    assert(defs.keySet == Set("html-term"))
    assert(definitionText(defs, "html-term") == "defined in HTML")
  }

  test("definitions are collected from every glossary-item, including across lists") {
    val xml: Xml.Element = parse(
      """<div>
        |<div class="glossary">
        |<dl>
        |<div class="glossary-item" id="posuk"><dt>posuk</dt><dd>verse</dd></div>
        |</dl>
        |</div>
        |<div class="glossary">
        |<dl>
        |<div class="glossary-item" id="mud"><dt>mud</dt><dd>wet dirt</dd></div>
        |<div class="glossary-item" id="rain"><dt>rain</dt><dd>water</dd></div>
        |</dl>
        |</div>
        |</div>
        |""".stripMargin
    )
    val defs: Map[String, Xml.Nodes] = Glossary.definitions(xml, HtmlXmlDialect)
    assert(defs.keySet == Set("posuk", "mud", "rain"))
    assert(definitionText(defs, "posuk") == "verse")
    assert(definitionText(defs, "mud") == "wet dirt")
    assert(definitionText(defs, "rain") == "water")
  }

  test("AsciiDoc with several [glossary] dlists yields definitions from each") {
    val xml: Xml.Element = fromAsciiDoc(
      """See <<posuk>>, <<rasha>>, <<cpu>>, <<mud>>, <<term-a>>, and <<term-b>>.
        |
        |== Hebrew terms
        |
        |[glossary]
        |[[posuk]]posuk:: verse
        |[[akdamus]]akdamus::
        |[[rasha]]rasha:: sinner
        |
        |== Computer terms
        |
        |[[cpu]]CPU:: processor
        |
        |== Dirt
        |
        |[glossary]
        |[[mud]]mud:: wet, cold dirt
        |[[rain]]rain:: water falling from the sky
        |
        |[glossary]
        |== Official glossary section
        |
        |[glossary]
        |[[term-a]]term-a:: definition a
        |
        |//-
        |
        |[glossary]
        |[[term-b]]term-b:: definition b
        |""".stripMargin
    )
    val defs: Map[String, Xml.Nodes] = Glossary.definitions(xml, HtmlXmlDialect)
    assert(defs.keySet == Set("posuk", "rasha", "mud", "rain", "term-a", "term-b"))
    assert(definitionText(defs, "posuk") == "verse")
    assert(definitionText(defs, "rasha") == "sinner")
    assert(definitionText(defs, "mud") == "wet, cold dirt")
    assert(definitionText(defs, "rain") == "water falling from the sky")
    assert(definitionText(defs, "term-a") == "definition a")
    assert(definitionText(defs, "term-b") == "definition b")
    val glossaryLists = HtmlXmlDialect.gather(xml, element =>
      Option.when(Glossary.isList(element))(element)
    )
    assert(glossaryLists.size == 4)
    val glossaryItems = HtmlXmlDialect.gather(xml, element =>
      Option.when(Glossary.isItem(element))(element.getId)
    ).flatten
    assert(glossaryItems.toSet == Set("posuk", "akdamus", "rasha", "mud", "rain", "term-a", "term-b"))
  }

  test("Markdown {:.glossary} IAL marks the preceding dl; unmarked lists are left alone") {
    val xml: Xml.Element = MarkdownMarkup.convert(parse(
      """<div>
        |<dl>
        |<dt>posuk</dt>
        |<dd><p>verse</p></dd>
        |</dl>
        |<p>{:.glossary}</p>
        |<dl>
        |<dt>CPU</dt>
        |<dd><p>processor</p></dd>
        |</dl>
        |</div>
        |""".stripMargin
    ))
    val rendered: String = render(xml)
    assert(rendered.contains("""class="glossary-item""""))
    assert(rendered.contains("""id="posuk""""))
    assert(!rendered.contains("dlist-item"))
    assert(!rendered.contains("{:.glossary}"))
    val defs: Map[String, Xml.Nodes] = Glossary.definitions(xml, HtmlXmlDialect)
    assert(defs.keySet == Set("posuk"))
    assert(definitionText(defs, "posuk") == "verse")
    assert(!defs.contains("CPU"))
  }

  test("Markdown dl with class glossary is converted without an IAL") {
    val xml: Xml.Element = MarkdownMarkup.convert(parse(
      """<div>
        |<dl class="glossary">
        |<dt>mud</dt>
        |<dd>wet dirt</dd>
        |</dl>
        |</div>
        |""".stripMargin
    ))
    val defs: Map[String, Xml.Nodes] = Glossary.definitions(xml, HtmlXmlDialect)
    assert(defs.keySet == Set("mud"))
    assert(definitionText(defs, "mud") == "wet dirt")
    assert(Glossary.isList(xml.getChildren.flatMap(_.asElement).find(_.getName == "dl").get))
  }

  test("Markdown dt id wins over the term-text slug") {
    val xml: Xml.Element = MarkdownMarkup.convert(parse(
      """<div>
        |<dl>
        |<dt id="custom">Alter Rebbe</dt>
        |<dd><p>the first Lubavitcher Rebbe</p></dd>
        |</dl>
        |<p>{: .glossary }</p>
        |</div>
        |""".stripMargin
    ))
    val defs: Map[String, Xml.Nodes] = Glossary.definitions(xml, HtmlXmlDialect)
    assert(defs.keySet == Set("custom"))
    assert(definitionText(defs, "custom") == "the first Lubavitcher Rebbe")
  }

  test("Markdown with several {:.glossary} lists yields definitions from each") {
    val xml: Xml.Element = fromMarkdown(
      """See [[#posuk]] and [mud](#mud).
        |
        |posuk
        |: verse
        |
        |rasha
        |: sinner
        |
        |{:.glossary}
        |
        |CPU
        |: processor
        |
        |## Dirt
        |
        |mud
        |: wet, cold dirt
        |
        |rain
        |: water falling from the sky
        |
        |{:.glossary}
        |""".stripMargin
    )
    val defs: Map[String, Xml.Nodes] = Glossary.definitions(xml, HtmlXmlDialect)
    assert(defs.keySet == Set("posuk", "rasha", "mud", "rain"))
    assert(definitionText(defs, "posuk") == "verse")
    assert(definitionText(defs, "rasha") == "sinner")
    assert(definitionText(defs, "mud") == "wet, cold dirt")
    assert(definitionText(defs, "rain") == "water falling from the sky")
    val glossaryLists = HtmlXmlDialect.gather(xml, element =>
      Option.when(Glossary.isList(element))(element)
    )
    assert(glossaryLists.size == 2)
    val rendered: String = render(xml)
    assert(!rendered.contains("dlist-item"))
    assert(!rendered.contains("{:.glossary}"))
    assert(rendered.contains("<dt>CPU</dt>"))
  }

  test("attachTip wraps the link and tip as siblings") {
    val link: Xml.Element = Xml
      .element("a")
      .setId("src")
      .setHref("#posuk")
      .setText("posuk")
    val withTip: Xml.Element = Glossary.tip.attachTip(link, Chunk(Xml.text("verse")))
    val rendered: String = render(withTip).replaceAll("\\s+", " ").replace("= ", "=")
    assert(Glossary.tip.isRef(withTip))
    assert(rendered.contains("""class="glossary-ref""""))
    assert(rendered.contains("""class="glossary-tip""""))
    assert(rendered.contains("""id="src-tip""""))
    assert(rendered.contains("""aria-describedby="src-tip""""))
    assert(rendered.contains("""role="tooltip""""))
    assert(rendered.contains(">posuk</a>"))
    assert(rendered.contains("verse"))
    assert(!rendered.contains("<a href=\"#posuk\">posuk<span"))
  }
