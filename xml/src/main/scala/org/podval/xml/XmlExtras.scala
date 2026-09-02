package org.podval.xml

import org.podval.xml.codec.{XmlCodec, XmlError}
import zio.blocks.schema.Schema

/** Unclaimed attributes and children of the parent element. Opt-in leftover capture. */
final case class XmlExtras(
  attributes: Seq[(String, String)] = Seq.empty,
  children: Seq[XmlNode] = Seq.empty
) derives CanEqual

object XmlExtras:
  given schema: Schema[XmlExtras] = Schema.derived

  val codec: XmlCodec[XmlExtras] = new XmlCodec[XmlExtras]:
    override def elementName: String = "extras"
    override def unsafeDecode[E: XmlAst](element: E): XmlExtras =
      throw XmlError("XmlExtras is decoded from leftover parent content, not from a child element")
    override def encodeNamed[E: XmlAst](name: String, value: XmlExtras): E =
      throw XmlError("XmlExtras is encoded into the parent element, not as a child")
