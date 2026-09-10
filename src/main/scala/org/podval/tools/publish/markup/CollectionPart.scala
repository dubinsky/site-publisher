package org.podval.tools.publish.markup

import org.podval.xml.{Xml, XmlCodec, given}
import zio.blocks.schema.{Modifier, Schema}

/** A collection `<part from="000">` title row. `from` is the first original document's file name. */
final case class CollectionPart(
  @Modifier.config(XmlCodec.Attribute, "") n: Option[String] = None,
  @Modifier.config(XmlCodec.Attribute, "") from: String,
  title: Option[Xml.Element] = None
) derives CanEqual

object CollectionPart:
  given schema: Schema[CollectionPart] = Schema.derived
  val codec: XmlCodec[CollectionPart] = XmlCodec.derived
