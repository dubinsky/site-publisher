package org.podval.tools.publish.markup

import org.podval.xml.{Xml, XmlAttribute, XmlElement, XmlParser}
import org.scalatest.funsuite.AnyFunSuite

final class Xml2HtmlSpec extends AnyFunSuite:
  private val tei: Xml2Html = Xml2Html("tei")

  private def parse(xml: String): Xml.Element =
    XmlParser.parseXml(xml).toOption.get

  test("elementName prefixes reserved tags; div stays") {
    assert(tei.elementName(XmlElement.P) == "tei-p")
    assert(tei.elementName(XmlElement.Head) == "tei-head")
    assert(tei.elementName(XmlElement.Title) == "tei-title")
    assert(tei.elementName(XmlElement.Body) == "tei-body")
    assert(tei.elementName(XmlElement.Div) == "div")
  }

  test("attributeName prefixes reserved names; xml:* and href stay") {
    assert(tei.attributeName(XmlAttribute.Lang) == "tei-lang")
    assert(tei.attributeName(XmlAttribute.CssClass) == "tei-class")
    assert(tei.attributeName(XmlAttribute.Target) == "tei-target")
    assert(tei.attributeName(XmlAttribute.Frame) == "tei-frame")
    assert(tei.attributeName(XmlAttribute.Href) == "href")
    assert(tei.attributeName(XmlAttribute.XmlLang) == "xml:lang")
    assert(tei.attributeName(XmlAttribute.XmlId) == "xml:id")
  }

  test("unprefixed reserved element is prefixed") {
    val html: Xml.Element = tei.convert(parse("<p>a</p>"))
    assert(html.isNamed(tei.elementName(XmlElement.P)))
    assert(tei.is(html, XmlElement.P))
    assert(html.getClasses.contains("p"))
  }

  test("prefixed reserved element is rewritten by local name") {
    val html: Xml.Element = tei.convert(
      parse("""<tei:p xmlns:tei="http://www.tei-c.org/ns/1.0">a</tei:p>""")
    )
    assert(html.isNamed(tei.elementName(XmlElement.P)))
    assert(tei.is(html, XmlElement.P))
    assert(html.getClasses.contains("p"))
  }

  test("reserved attributes use local names; xml:lang and xml:id stay") {
    val raw: Xml.Element = parse("""<p xml:id="n1" xml:lang="en" lang="fr" class="x" frame="box"/>""")
    assert(tei.is(raw, XmlElement.P))
    assert(tei.get(raw, XmlAttribute.Lang).contains("fr"))
    assert(tei.get(raw, XmlAttribute.CssClass).contains("x"))
    assert(tei.get(raw, XmlAttribute.Frame).contains("box"))

    val html: Xml.Element = tei.convert(raw)
    assert(html.get(XmlAttribute.XmlId).contains("n1"))
    assert(html.get(XmlAttribute.XmlLang).contains("en"))
    assert(html.get(XmlAttribute.Lang).isEmpty)
    assert(html.getClasses.contains("p"))
    assert(tei.get(html, XmlAttribute.Lang).contains("fr"))
    assert(tei.get(html, XmlAttribute.CssClass).contains("x"))
    assert(tei.get(html, XmlAttribute.Frame).contains("box"))
    assert(tei.get(html, XmlAttribute.XmlLang).contains("en"))
  }

  test("is matches catalog name before convert and rewritten name after") {
    val title: Xml.Element = parse("<title>a</title>")
    assert(tei.is(title, XmlElement.Title))
    assert(!tei.is(title, XmlElement.Head))
    val html: Xml.Element = tei.convert(title)
    assert(tei.is(html, XmlElement.Title))
    assert(html.isNamed("tei-title"))
  }
