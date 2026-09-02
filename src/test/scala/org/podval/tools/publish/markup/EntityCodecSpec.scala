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
    val decoded: Entity = Entity.codec.decode(parse(xml)).toOption.get
    assert(decoded.kind == EntityKind.Person)
    assert(decoded.id.contains("x"))
    assert(decoded.role.contains("jew"))
    assert(decoded.names.map(_.name) == Seq("Залман Борухович", "Залман"))
    assert(decoded.names.forall(_.kind == EntityKind.Person))
    assert(decoded.extras.children.collect { case XmlNode.Element(name, _, _) => name } == Seq("p"))
    val encoded: Xml.Element = Entity.codec.encode(decoded)
    assert(encoded.getName == "person")
    assert(encoded.get("id").contains("x"))
    assert(encoded.getChildren.flatMap(_.asElement).map(_.getName) == Seq("persName", "persName", "p"))
  }

  test("place tag sets kind and encodes placeName") {
    val decoded: Entity = Entity.codec.decode(parse("""<place><placeName>Вильна</placeName></place>""")).toOption.get
    assert(decoded.kind == EntityKind.Place)
    assert(decoded.names.map(_.name) == Seq("Вильна"))
    assert(decoded.names.head.kind == EntityKind.Place)
    val encoded: Xml.Element = Entity.codec.encode(decoded)
    assert(encoded.getName == "place")
    assert(encoded.getChildren.flatMap(_.asElement).map(_.getName) == Seq("placeName"))
  }

  test("constructed place encodes from kind") {
    val entity: Entity = Entity(
      kind = EntityKind.Place,
      names = Seq(EntityName(kind = EntityKind.Place, name = "Вильна"))
    )
    val encoded: Xml.Element = Entity.codec.encode(entity)
    assert(encoded.getName == "place")
    assert(encoded.getChildren.flatMap(_.asElement).map(_.getName) == Seq("placeName"))
  }

  test("entity reference keeps mixed leftover content") {
    val xml: String = """<persName ref="alter-rebbe">the <hi>Rebbe</hi></persName>"""
    val decoded: EntityReference = EntityReference.codec.decode(parse(xml)).toOption.get
    assert(decoded.kind == EntityKind.Person)
    assert(decoded.ref.contains("alter-rebbe"))
    assert(decoded.extras.children.exists:
      case XmlNode.Text(value) => value.contains("the")
      case _ => false
    )
    assert(decoded.extras.children.collect { case XmlNode.Element(name, _, _) => name } == Seq("hi"))
    val encoded: Xml.Element = EntityReference.codec.encode(decoded)
    assert(encoded.getName == "persName")
    assert(encoded.get("ref").contains("alter-rebbe"))
  }

  test("entity lists decode mixed kinds from element names") {
    val xml: String =
      """<entityLists>
        |  <title>Имена</title>
        |  <listPerson n="jews" role="jew"><title>Жиды</title></listPerson>
        |  <listPlace n="places"><title>Места</title></listPlace>
        |</entityLists>""".stripMargin
    val index: EntityLists.Index = EntityLists.harvest(parse(xml)).get
    assert(index.title.contains("Имена"))
    assert(index.lists.map(_.kind) == Seq(EntityKind.Person, EntityKind.Place))
    val encoded: Xml.Element = EntityLists.Index.codec.encode(index)
    assert(encoded.getChildren.flatMap(_.asElement).map(_.getName) ==
      Seq("title", "listPerson", "listPlace"))
  }
