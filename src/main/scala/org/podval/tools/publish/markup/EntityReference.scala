package org.podval.tools.publish.markup

import org.podval.xml.{XmlCodec, XmlExtras}
import zio.blocks.schema.{Modifier, Schema}
import zio.blocks.typeid.TypeId

final case class EntityReference(
//  @Modifier.config(XmlCodec.Attribute, "") kind: EntityKind,
  @Modifier.config(XmlCodec.Attribute, "") id: Option[String] = None,
  @Modifier.config(XmlCodec.Attribute, "") role: Option[String] = None,
  @Modifier.config(XmlCodec.Attribute, "") ref: Option[String] = None,
  extras: XmlExtras = XmlExtras()
) derives CanEqual

object EntityReference:
  given schema: Schema[EntityReference] = Schema.derived
  val codec: XmlCodec[EntityReference] = XmlCodec.derived

  def codec(kind: EntityKind): XmlCodec[EntityReference] = schema
    .deriving(XmlCodec.deriver)
    .modifier(TypeId.of[EntityReference], Modifier.config(XmlCodec.Element, kind.nameElement))
    .derive
