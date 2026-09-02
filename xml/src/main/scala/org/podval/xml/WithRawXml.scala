package org.podval.xml

import zio.blocks.chunk.Chunk
import zio.blocks.schema.Schema
import zio.blocks.schema.derive.Deriver
import zio.blocks.schema.xml.{Xml, XmlCodec, XmlFormat, XmlName}

abstract class WithRawXml:
  var rawAttributes: Chunk[(XmlName, String)] = Chunk.empty
  var rawChildren: Chunk[Xml] = Chunk.empty

object WithRawXml:
  val dummyElement: String = "idioticDummyElementToForceZioBlocksXmlToNotDiscardAttributes"

  def codec[A <: WithRawXml](
    schema: Schema[A],
    codecDeriver: Option[Deriver[XmlCodec]] = None
  ): XmlCodec[A] = new XmlCodec[A]:
    val codec: XmlCodec[A] = schema.derive(codecDeriver.getOrElse(XmlFormat.deriver))

    val fieldNames: Set[String] = schema.reflect.asRecord.get.fields.map(_.name).toSet
    def isParsed(xmlName: XmlName): Boolean = fieldNames.contains(xmlName.qualifiedName)

    def decodeValue(xml: Xml): A =
      val element: Xml.Element = xml match
        case element: Xml.Element => element
        case _ => error("Expected an element")

      val result: A = codec.decodeValue(element)

      result.rawAttributes = element.attributes.filterNot(
        attribute => isParsed(attribute._1)
      )

      result.rawChildren = element.children.filterNot {
        case element: Xml.Element => isParsed(element.name) || element.name.localName == dummyElement
        case _ => false
      }

      result

    def encodeValue(withRawXml: A): Xml.Element =
      val result: Xml.Element = codec.encodeValue(withRawXml).asInstanceOf[Xml.Element]

      result.copy(
        attributes = result.attributes ++ withRawXml.rawAttributes,
        children = result.children ++ withRawXml.rawChildren
      )
