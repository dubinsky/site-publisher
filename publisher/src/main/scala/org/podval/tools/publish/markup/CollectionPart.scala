package org.podval.tools.publish.markup

import org.podval.xml.{Xml, XmlCodec}
import XmlCodec.given
import zio.blocks.schema.{Modifier, Schema}

/** A collection `<part from="000">` title row. `from` is the first original document's file name. */
@Modifier.config(XmlCodec.Element, "part")
final case class CollectionPart(
  n: Option[String] = None,
  from: String,
  title: Option[Xml.Element] = None
) derives CanEqual

object CollectionPart:
  given schema: Schema[CollectionPart] = Schema.derived
  val codec: XmlCodec[CollectionPart] = XmlCodec.derived
