package org.podval.tools.publish.markup

import org.podval.xml.{XmlCodec, XmlExtras}
import zio.blocks.schema.{Modifier, Schema}

final case class EntityReference(
  kind: EntityKind,
  @Modifier.config(XmlCodec.Attribute, "") id: Option[String] = None,
  @Modifier.config(XmlCodec.Attribute, "") role: Option[String] = None,
  @Modifier.config(XmlCodec.Attribute, "") ref: Option[String] = None,
  extras: XmlExtras = XmlExtras()
) derives CanEqual

object EntityReference:
  given schema: Schema[EntityReference] = Schema.derived
  val codec: XmlCodec[EntityReference] = XmlCodec.derived[EntityReference, EntityKind]("kind", EntityKind.asName)
