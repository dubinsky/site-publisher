package org.podval.tools.publish.markup

import org.podval.metadata.{Language, Name}
import org.podval.xml.{Xml, XmlAttribute, XmlCodec}
import XmlCodec.given
import zio.blocks.schema.{Modifier, Schema}

/** TEI `store` / `collection`. `hrefs` are page references, not XInclude.
  * Bind (before wrap) and wrap (`StoreTree`) read this; header chrome and collection
  * listings use the wrapped tree. Collection `part`s and `pageType` feed `CollectionIndex`. */
@Modifier.config(XmlCodec.IgnoreUnknown, "")
final case class StoreIndex(
  @Modifier.config(XmlCodec.Attribute, "") n: Option[String] = None,
  @Modifier.config(XmlCodec.Attribute, "") alias: Option[String] = None,
  @Modifier.config(XmlCodec.Attribute, "pageType") pageTypeName: Option[String] = None,
  @Modifier.config(XmlCodec.Element, "name") names: Seq[Name] = Seq.empty,
  @Modifier.config(XmlCodec.Element, "title") titles: Seq[Xml.Element] = Seq.empty,
  @Modifier.config(XmlCodec.Element, "abstract") description: Option[Xml.Element] = None,
  body: Option[Xml.Element] = None,
  @Modifier.config(XmlCodec.Element, "part") parts: Seq[CollectionPart] = Seq.empty,
  @Modifier.config(XmlCodec.Element, "by") axis: Option[StoreIndex.Axis] = None,
  @Modifier.config(XmlCodec.Include, "") hrefs: Seq[String] = Seq.empty,
  isCollection: Boolean = false
) derives CanEqual:
  def selector: Option[String] = axis.flatMap(_.selector)

  def pageType: PageType = PageType.parse(pageTypeName)

  def title: Option[Xml.Element] =
    val nonempty: Seq[Xml.Element] = titles.filter(_.getText.trim.nonEmpty)
    nonempty.find(_.get(XmlAttribute.Type).contains("main")).orElse(nonempty.headOption)

  def displayName(lang: String): Option[String] =
    names.find(_.languageSpec.language.exists(_.name == lang)).orElse(names.headOption).map(_.name)

object StoreIndex:
  @Modifier.config(XmlCodec.IgnoreUnknown, "")
  final case class Axis(
    @Modifier.config(XmlCodec.Attribute, "") selector: Option[String] = None
  ) derives CanEqual

  object Axis:
    given schema: Schema[Axis] = Schema.derived

  given schema: Schema[StoreIndex] = Schema.derived
  val codec: XmlCodec[StoreIndex] = XmlCodec.derived

  def apply(xml: Xml.Element): Option[StoreIndex] =
    Option.when(TeiMarkup.isStoreRoot(xml)):
      val decoded: StoreIndex = codec.unsafeDecode(xml)
      val mergedNames: Seq[Name] =
        if decoded.names.nonEmpty then decoded.names
        else decoded.n.map(_.trim).filter(_.nonEmpty).map(Name(_, Language.Spec.empty)).toSeq
      decoded.copy(
        names = mergedNames,
        isCollection = xml.isNamed("collection"),
        description = decoded.description.filter(_.getChildren.nonEmpty),
        body = decoded.body.filter(_.getChildren.nonEmpty)
      )
