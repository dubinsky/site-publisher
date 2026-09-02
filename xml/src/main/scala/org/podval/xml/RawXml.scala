package org.podval.xml

import zio.blocks.schema.xml.{Xml, XmlCodec}

// I need to be able to work with *partially* decoded XML trees,
// with some subtrees held as raw XML.
// The only thing I need to be able to do with such undecoded subtrees is
// insert them into the result on encoding.
// This class serves this need:
// a field of type `RawXml` gets decoded from an XML element
// with the name corresponding to the field name,
// so name check is done by the core XML Codec;
// instance of this class holds the undecoded element until
// encoding, when it incorporates it into the output as-is.

// To use, register `RawXml.codec` with the Codec Deriver:
//  private val codec: XmlCodec[YOUR-CLASS] = schema
//    .deriving(XmlFormat.deriver)
//    .instance(TypeId.of[RawXml], RawXml.codec)
//    .derive

// I have to use a member and not a constructor parameter to hold
// the undecoded XML tree, otherwise ZIO Block Schema deriver chokes with:
//    Found:    zio.blocks.schema.Schema[IndexedSeq[(zio.blocks.schema.xml.XmlName, String)]]
//    Required: zio.blocks.schema.Schema[zio.blocks.chunk.Chunk[(zio.blocks.schema.xml.XmlName, String)]]
//
//    The following import might make progress towards fixing the problem:
//      import zio.blocks.schema.OpticsFor.derive
//      given schema: Schema[Tei] = Schema.derived
final class RawXml:
  private var elementVar: Option[Xml.Element] = None
  def set(element: Xml.Element): Unit = elementVar = Some(element)
  def get: Xml.Element = elementVar.get

  override def toString: String = s"RawXml(${get.name.qualifiedName})"

object RawXml:
  def apply(element: Xml.Element): RawXml =
    val result = new RawXml
    result.set(element)
    result

  def codec: XmlCodec[RawXml] = new XmlCodec[RawXml]:
    def encodeValue(rawXml: RawXml): Xml.Element = rawXml.get

    def decodeValue(xml: Xml): RawXml = xml match
      case element: Xml.Element => RawXml(element)
      case _ => error("Expected an element")



