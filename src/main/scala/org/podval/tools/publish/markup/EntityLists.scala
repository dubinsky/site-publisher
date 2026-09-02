package org.podval.tools.publish.markup

import org.podval.xml.{Xml, XmlCodec}
import zio.blocks.schema.{Modifier, Schema}

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
  // TODO can this be collapsed?!
  sealed trait Spec derives CanEqual:
    def id: String
    def role: Option[String]
    def title: String
    def kind: EntityKind

  @Modifier.config(XmlCodec.Element, "listPerson")
  final case class ListPerson(
    @Modifier.config(XmlCodec.Attribute, "n") id: String,
    @Modifier.config(XmlCodec.Attribute, "") role: Option[String] = None,
    @Modifier.config(XmlCodec.Element, "title")
    @Modifier.alias("tei-title")
    title: String
  ) extends Spec derives CanEqual:
    def kind: EntityKind = EntityKind.Person

  @Modifier.config(XmlCodec.Element, "listPlace")
  final case class ListPlace(
    @Modifier.config(XmlCodec.Attribute, "n") id: String,
    @Modifier.config(XmlCodec.Attribute, "") role: Option[String] = None,
    @Modifier.config(XmlCodec.Element, "title")
    @Modifier.alias("tei-title")
    title: String
  ) extends Spec derives CanEqual:
    def kind: EntityKind = EntityKind.Place

  @Modifier.config(XmlCodec.Element, "listOrg")
  final case class ListOrg(
    @Modifier.config(XmlCodec.Attribute, "n") id: String,
    @Modifier.config(XmlCodec.Attribute, "") role: Option[String] = None,
    @Modifier.config(XmlCodec.Element, "title")
    @Modifier.alias("tei-title")
    title: String
  ) extends Spec derives CanEqual:
    def kind: EntityKind = EntityKind.Organization

  object Spec:
    given schema: Schema[Spec] = Schema.derived

  final case class Index(
    @Modifier.config(XmlCodec.Element, "title")
    @Modifier.alias("tei-title")
    title: Option[String] = None,
    lists: Seq[Spec] = Seq.empty
  ) derives CanEqual

  object Index:
    given schema: Schema[Index] = Schema.derived
    val codec: XmlCodec[Index] = XmlCodec.derived

  def harvest(xml: Xml.Element): Option[Index] =
    Option.when(xml.localName == "entityLists"):
      Index.codec.decode(xml).fold(err => throw err, identity)
