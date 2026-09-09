package org.podval.tools.publish.markup

import org.podval.xml.{Xml, XmlCodec}
import zio.blocks.schema.{Modifier, Schema}
import zio.blocks.typeid.TypeId

/** TEI `entityLists` directory index specs: kind + role buckets.
  * Harvested from the raw tree; member lists are generated in `page.EntityLists`.
  * The names directory page is `By("names")`; each list page is `By("name")`. */
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
    Option.when(xml.isNamed("entityLists")):
      Index.codec.decode(xml).fold(err => throw err, identity)
