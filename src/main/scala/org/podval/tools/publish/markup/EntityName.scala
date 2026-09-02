package org.podval.tools.publish.markup

import org.podval.xml.XmlCodec
import zio.blocks.schema.{Modifier, Schema}

final case class EntityName(
  kind: EntityKind,
  @Modifier.config(XmlCodec.Attribute, "") id: Option[String] = None,
  @Modifier.config(XmlCodec.Attribute, "") ref: Option[String] = None,
  @Modifier.config(XmlCodec.Text, "") name: String
) derives CanEqual

object EntityName:
  given schema: Schema[EntityName] = Schema.derived
  val codec: XmlCodec[EntityName] = XmlCodec.derived[EntityName, EntityKind]("kind", EntityKind.asName)
