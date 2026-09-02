package org.podval.tools.publish.markup

import org.podval.xml.{XmlCodec, XmlCodecDeriver, XmlExtras}
import zio.blocks.schema.{Modifier, Schema}
import zio.blocks.typeid.TypeId

//final class Entity(
//  val entityType: EntityType,
//  val role: Option[String],
//  override val name: String,
//  val mainName: String  // Note: can mostly be reconstructed from the name...
//)
final case class Entity(
  kind: EntityKind,
  @Modifier.config(XmlCodec.Attribute, "") id: Option[String] = None,
  @Modifier.config(XmlCodec.Attribute, "") role: Option[String] = None,
  names: Seq[EntityName] = Seq.empty,
  extras: XmlExtras = XmlExtras()
) derives CanEqual

object Entity:
  given schema: Schema[Entity] = Schema.derived

  val codec: XmlCodec[Entity] = schema
    .deriving(XmlCodecDeriver.tagged[Entity, EntityKind]("kind", EntityKind.asRoot))
    // Schema re-derives nested types with this deriver; tagged() applies only to Entity.
    // Without this, EntityName.kind would be a child element, not persName/placeName/orgName.
    .instance(TypeId.of[EntityName], EntityName.codec)
    .derive
