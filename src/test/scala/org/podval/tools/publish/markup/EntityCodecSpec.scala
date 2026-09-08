package org.podval.tools.publish.markup

import org.podval.xml.{Xml, XmlParser}
import org.scalatest.funsuite.AnyFunSuite

final class EntityCodecSpec extends AnyFunSuite:
  private def parse(xml: String): Xml.Element = XmlParser.parseXml(xml).toOption.get

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
    assert(encoded.getChildren.flatMap(_.asElement).map(_.qName) ==
      Seq("title", "listPerson", "listPlace"))
  }
