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
    assert(encoded.childElements.map(_.getName.qName) ==
      Seq("title", "listPerson", "listPlace"))
  }

  test("list head decodes as the list title and encodes as title") {
    val xml: String =
      """<entityLists xmlns="http://www.tei-c.org/ns/1.0">
        |  <listPerson n="jews" role="jew"><head>Жиды</head></listPerson>
        |</entityLists>""".stripMargin
    val index: EntityLists.Index = EntityLists.harvest(parse(xml)).get
    assert(index.lists.map(_.title) == Seq("Жиды"))
    val encoded: Xml.Element = EntityLists.Index.codec.encode(index)
    assert(encoded.childElements.flatMap(_.childElements).map(_.getName.qName) == Seq("head"))
  }
