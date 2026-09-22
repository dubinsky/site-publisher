package org.podval.tools.publish.markup

import org.podval.xml.{Xml, XmlCodec}
import Xml.given
import zio.blocks.schema.{Modifier, Schema}

/** TEI `entityLists` catalog specs: kind + role buckets (site-wide members).
  * Harvested from the raw tree; member lists are generated in `page.EntityLists`.
  * The catalog page is `By("names")`; each list page is `By("name")`. */
object EntityLists:
  final case class Index(
    @Modifier.config(XmlCodec.Element, "title")
    @Modifier.alias("tei-title")
    title: Option[String] = None,
    lists: Seq[EntityList] = Seq.empty
  ) derives CanEqual

  object Index:
    given schema: Schema[Index] = Schema.derived
    val codec: XmlCodec[Index] = XmlCodec.derived(EntityList.codec)

  def harvest(xml: Xml.Element): Option[Index] =
    Option.when(xml.isNamed("entityLists")):
      Index.codec.decode(xml).fold(err => throw err, identity)
