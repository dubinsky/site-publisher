package org.podval.tools.publish.markup

import org.podval.xml.XmlExtras
import org.podval.xml.codec.XmlCodec
import zio.blocks.schema.{Modifier, Schema}
import zio.blocks.typeid.TypeId

//final class Entity(
//  val entityType: EntityType,
//  val role: Option[String],
//  override val name: String,
//  val mainName: String  // Note: can mostly be reconstructed from the name...
//)
final case class Entity(
  @Modifier.config(XmlCodec.Attribute, "") id: Option[String] = None,
//  val entityType: EntityType,
  @Modifier.config(XmlCodec.Attribute, "") role: Option[String] = None,
  @Modifier.config(XmlCodec.Element, "persName")
  @Modifier.alias("placeName")
  @Modifier.alias("orgName")
  names: Seq[EntityName] = Seq.empty,
  extras: XmlExtras = XmlExtras()
) derives CanEqual

object Entity:
  given schema: Schema[Entity] = Schema.derived

  val codec: XmlCodec[Entity] = XmlCodec.derived

  def codec(kind: EntityKind): XmlCodec[Entity] =
    schema
      .deriving(XmlCodec.deriver)
      .modifier(TypeId.of[Entity], Modifier.config(XmlCodec.Element, kind.element))
      .modifier(TypeId.of[Entity], "names", Modifier.config(XmlCodec.Element, kind.nameElement))
      .derive
