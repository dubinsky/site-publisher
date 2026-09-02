package org.podval.tools.publish.markup

import org.podval.xml.{Xml, XmlCodec, XmlNode, XmlParser}
import org.scalatest.funsuite.AnyFunSuite

final class EntityCodecSpec extends AnyFunSuite:
  private def parse(xml: String): Xml.Element = XmlParser.parseXml(xml).toOption.get

  test("person names and leftover body go to extras") {
    val xml: String =
      """<person id="x" role="jew">
        |  <persName>Залман Борухович</persName>
        |  <persName>Залман</persName>
        |  <p>founder</p>
        |</person>""".stripMargin
    val decoded: Entity = Entity.codec(EntityKind.Person).decode(parse(xml)).toOption.get
    assert(decoded.id.contains("x"))
    assert(decoded.role.contains("jew"))
    assert(decoded.names.map(_.name) == Seq("Залман Борухович", "Залман"))
    assert(decoded.extras.children.collect { case XmlNode.Element(name, _, _) => name } == Seq("p"))
    val encoded: Xml.Element = Entity.codec(EntityKind.Person).encode(decoded)
    assert(encoded.getName == "person")
    assert(encoded.get("id").contains("x"))
    assert(encoded.getChildren.flatMap(_.asElement).map(_.getName) == Seq("persName", "persName", "p"))
  }

  test("placeName aliases decode without a kind-specific codec") {
    val decoded: Entity = Entity.codec.decode(parse("""<place><placeName>Вильна</placeName></place>""")).toOption.get
    assert(decoded.names.map(_.name) == Seq("Вильна"))
  }

  test("kind-specific codec encodes placeName") {
    val entity: Entity = Entity(names = Seq(EntityName(name = "Вильна")))
    val encoded: Xml.Element = Entity.codec(EntityKind.Place).encode(entity)
    assert(encoded.getName == "place")
    assert(encoded.getChildren.flatMap(_.asElement).map(_.getName) == Seq("placeName"))
  }

  test("entity reference keeps mixed leftover content") {
    val xml: String = """<persName ref="alter-rebbe">the <hi>Rebbe</hi></persName>"""
    val decoded: EntityReference = EntityReference.codec.decode(parse(xml)).toOption.get
    assert(decoded.ref.contains("alter-rebbe"))
    assert(decoded.extras.children.exists:
      case XmlNode.Text(value) => value.contains("the")
      case _ => false
    )
    assert(decoded.extras.children.collect { case XmlNode.Element(name, _, _) => name } == Seq("hi"))
    val encoded: Xml.Element = EntityReference.codec(EntityKind.Person).encode(decoded)
    assert(encoded.getName == "persName")
    assert(encoded.get("ref").contains("alter-rebbe"))
  }
