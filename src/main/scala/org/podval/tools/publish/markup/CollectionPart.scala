package org.podval.tools.publish.markup

import org.podval.xml.{Xml, XmlCodec}
import zio.blocks.schema.{Modifier, Schema}

/** A collection `<part from="000">` title row. `from` is the first original document's file name. */
final case class CollectionPart(
  @Modifier.config(XmlCodec.Attribute, "") n: Option[String] = None,
  @Modifier.config(XmlCodec.Attribute, "") from: String,
  // TODO why do I need Element modifier here?
  @Modifier.config(XmlCodec.Element, "title") title: Option[Xml.Element] = None
) derives CanEqual

object CollectionPart:
  import XmlCodec.xmlElementSchema
  given schema: Schema[CollectionPart] = Schema.derived
  val codec: XmlCodec[CollectionPart] = XmlCodec.derived

  def harvest(xml: Xml.Element): Seq[CollectionPart] =
    xml.getChildren.flatMap(_.asElement).filter(_.localName == "part").flatMap: element =>
      codec.decode(element).toOption
