package org.podval.tools.publish.markup

import org.podval.xml.{HtmlXmlDialect, Xml, XmlParser}
import org.scalatest.funsuite.AnyFunSuite

final class AsciiDocSpec extends AnyFunSuite:
  private def cleanup(input: String): String =
    val parsed = XmlParser.parseXml(input).toOption.get
    HtmlXmlDialect.render(AsciiDocMarkup.cleanup(parsed))

  private def isItem(rendered: String, id: String, cssClass: String): Boolean =
    rendered.contains(s"""<div class="$cssClass" id="$id">""") ||
    rendered.contains(s"""<div id="$id" class="$cssClass">""")

  test("glossary term ids are hoisted onto glossary-item and each term is grouped") {
    val rendered: String = cleanup(
      """<div class="dlist glossary">
        |<dl>
        |<dt><a id="posuk"></a>posuk</dt>
        |<dd><p>verse</p></dd>
        |<dt><a id="akdamus"></a>akdamus</dt>
        |<dt><a id="rasha"></a>rasha</dt>
        |<dd><p>sinner</p></dd>
        |</dl>
        |</div>
        |""".stripMargin
    )
    assert(isItem(rendered, "posuk", "glossary-item"))
    assert(isItem(rendered, "akdamus", "glossary-item"))
    assert(isItem(rendered, "rasha", "glossary-item"))
    assert(rendered.contains("""class="glossary""""))
    assert(!rendered.contains("dlist"))
    assert(!rendered.contains("dlist-item"))
    assert(rendered.contains("<dt>posuk</dt>"))
    assert(rendered.contains("<dt>akdamus</dt>"))
    assert(rendered.contains("<dt>rasha</dt>"))
    assert(rendered.contains("<dd>verse</dd>"))
    assert(rendered.contains("<dd>sinner</dd>"))
    assert(!rendered.contains("<dd><p>"))
    assert(!rendered.contains("""<a id="posuk">"""))
    assert(!rendered.contains("""<a id="akdamus">"""))
    assert(!rendered.contains("""<a id="rasha">"""))
    assert(!rendered.contains("""<dt id="posuk">"""))
    assert(!rendered.contains("""<dt id="rasha">"""))
  }

  test("non-glossary description lists are left as Asciidoctor emitted them") {
    val rendered: String = cleanup(
      """<div class="dlist">
        |<dl>
        |<dt><a id="cpu"></a>CPU</dt>
        |<dd><p>processor</p></dd>
        |</dl>
        |</div>
        |""".stripMargin
    )
    assert(rendered.contains("""<a id="cpu">"""))
    assert(rendered.contains("<dt>"))
    assert(rendered.contains("<dd>processor</dd>"))
    assert(!rendered.contains("glossary-item"))
    assert(!rendered.contains("dlist-item"))
    assert(!isItem(rendered, "cpu", "glossary-item"))
  }

  test("empty id anchors with href are left in place") {
    val rendered: String = cleanup(
      """<div>
        |<dl>
        |<dt><a id="stay" href="#other"></a>term</dt>
        |<dd><p>def</p></dd>
        |</dl>
        |</div>
        |""".stripMargin
    )
    assert(rendered.contains("""<a id="stay" href="#other">"""))
    assert(rendered.contains("<dd>def</dd>"))
    assert(!rendered.contains("<dd><p>"))
    assert(!rendered.contains("""<dt id="stay">"""))
    assert(!isItem(rendered, "stay", "glossary-item"))
  }
