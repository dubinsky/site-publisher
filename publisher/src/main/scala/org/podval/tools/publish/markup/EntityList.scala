package org.podval.tools.publish.markup

import org.podval.xml.XmlCodec
import zio.blocks.schema.{Modifier, Schema}

final case class EntityList(
  kind: EntityKind,
  @Modifier.rename("n") id: String,
  role: Option[String] = None,
  @Modifier.config(XmlCodec.Element, "title")
  @Modifier.alias("tei-title")
  title: String
) derives CanEqual

object EntityList:
  given schema: Schema[EntityList] = Schema.derived
  val codec: XmlCodec[EntityList] = XmlCodec.derived[EntityList, EntityKind]("kind", EntityKind.asList)
