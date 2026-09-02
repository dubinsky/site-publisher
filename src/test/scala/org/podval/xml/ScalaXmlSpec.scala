package org.podval.xml

import org.scalatest.funsuite.AnyFunSuite

final class ScalaXmlSpec extends AnyFunSuite:
  test("element uses the given name") {
    val el: ScalaXml.Element = ScalaXml.element("p")
    assert(ScalaXml.getName(el) == "p")
    assert(el.label == "p")
    assert(el.prefix == null)
  }

  test("qualified element names round-trip prefix and label") {
    val el: ScalaXml.Element = ScalaXml.element("tei:p")
    assert(ScalaXml.getName(el) == "tei:p")
    assert(el.prefix == "tei")
    assert(el.label == "p")
    val renamed: ScalaXml.Element = ScalaXml.rename(el)("div")
    assert(ScalaXml.getName(renamed) == "div")
    assert(renamed.prefix == null)
    assert(renamed.label == "div")
  }

  test("getName reconstructs prefix from an existing Elem") {
    val existing: scala.xml.Elem = scala.xml.Elem(
      "tei",
      "p",
      scala.xml.Null,
      scala.xml.TopScope,
      false
    )
    assert(ScalaXml.getName(existing) == "tei:p")
  }

  test("attributes preserve order and prefixes") {
    val el: ScalaXml.Element = ScalaXml.setAttributes(ScalaXml.element("p"))(Seq(
      "id" -> "a",
      "xml:id" -> "b",
      "class" -> "c"
    ))
    assert(ScalaXml.getAttributes(el) == Seq("id" -> "a", "xml:id" -> "b", "class" -> "c"))
  }

  test("cdata is PCData") {
    val node: ScalaXml.Node = ScalaXml.cdata("a<b")
    assert(ScalaXml.asCData(node).contains("a<b"))
    assert(ScalaXml.asAtom(node).contains("a<b"))
    assert(ScalaXml.asText(node).isEmpty)
  }

  test("Ast2Ast round-trips Xml through ScalaXml") {
    object XmlToScalaXml extends Ast2Ast(Xml, ScalaXml)
    object ScalaXmlToXml extends Ast2Ast(ScalaXml, Xml)
    val xml: Xml.Element = XmlParser.parseXml("""<p xml:id="x"><q>a</q>b</p>""").toOption.get
    val round: Xml.Element = ScalaXmlToXml.convert(XmlToScalaXml.convert(xml))
    assert(round.getName == "p")
    assert(round.get("xml:id").contains("x"))
    assert(round.getChildren.flatMap(_.asElement).map(_.getName) == Seq("q"))
    assert(round.getChildren.flatMap(_.asText) == Seq("b"))
  }

  test("Ast2Ast round-trips CDATA through ScalaXml") {
    object XmlToScalaXml extends Ast2Ast(Xml, ScalaXml)
    object ScalaXmlToXml extends Ast2Ast(ScalaXml, Xml)
    val xml: Xml.Element = Xml.element("p").setChildren(Seq(Xml.cdata("a<b")))
    val scalaXml: ScalaXml.Element = XmlToScalaXml.convert(xml)
    assert(ScalaXml.getChildren(scalaXml).flatMap(ScalaXml.asCData) == Seq("a<b"))
    val round: Xml.Element = ScalaXmlToXml.convert(scalaXml)
    assert(round.getChildren.flatMap(_.asCData) == Seq("a<b"))
  }
