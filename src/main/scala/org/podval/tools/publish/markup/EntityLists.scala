package org.podval.tools.publish.markup

import org.podval.xml.{Xml, XmlCodec}
import zio.blocks.schema.{Modifier, Schema}
import zio.blocks.typeid.TypeId

//// TODO derive it from By (with a transparent Selector)!
//final class EntityList(
//  override val fromUrl: FromUrl,
//  override val names: Names,
//  val entityType: EntityType,
//  val role: Option[String],
//  val title: Title.Value,
//) extends
//  Pure[Entity]

/** TEI `entityLists` directory index specs: kind + role buckets.
  * Harvested from the raw tree; member lists are generated in `page.EntityLists`. */
object EntityLists:
  final case class Index(
    @Modifier.config(XmlCodec.Element, "title")
    @Modifier.alias("tei-title")
    title: Option[String] = None,
    lists: Seq[EntityList] = Seq.empty
  ) derives CanEqual

  object Index:
    given schema: Schema[Index] = Schema.derived
    val codec: XmlCodec[Index] = schema
      .deriving(XmlCodec.deriver)
      // Schema re-derives nested types with this deriver. EntityList.codec is tagged
      // (listPerson/listPlace/listOrg); without this, items would encode as <EntityList>.
      .instance(TypeId.of[EntityList], EntityList.codec)
      .derive

  def harvest(xml: Xml.Element): Option[Index] =
    Option.when(xml.localName == "entityLists"):
      Index.codec.decode(xml).fold(err => throw err, identity)
