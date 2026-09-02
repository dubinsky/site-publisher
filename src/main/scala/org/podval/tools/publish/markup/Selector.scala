package org.podval.tools.publish.markup

import org.podval.xml.{XmlCodec, XmlParser}
import zio.blocks.schema.{Modifier, Schema}

/** A store/collection `by/@selector` (or a document facet). Loaded from `Selector.xml`
  * (same catalog as the old Open Torah collector). Display name prefers Russian. */
final case class Selector(
  @Modifier.config(XmlCodec.Element, "name") names: Seq[Selector.Name],
  @Modifier.config(XmlCodec.Attribute, "") title: Option[String] = None
) derives CanEqual:
  def displayName: String =
    names.find(_.lang.contains("ru")).orElse(names.headOption).map(_.n).getOrElse("")

  def matches(n: String): Boolean =
    names.exists(_.n.equalsIgnoreCase(n))

object Selector:
  final case class Name(
    @Modifier.config(XmlCodec.Attribute, "") n: String,
    @Modifier.config(XmlCodec.Attribute, "") lang: Option[String] = None,
    @Modifier.config(XmlCodec.Attribute, "") transliterated: Option[Boolean] = None
  ) derives CanEqual

  object Name:
    given schema: Schema[Name] = Schema.derived

  given schema: Schema[Selector] = Schema.derived
  val codec: XmlCodec[Selector] = XmlCodec.derived

  def displayName(n: String): String = find(n).map(_.displayName).getOrElse(n)

  def find(n: String): Option[Selector] = all.find(_.matches(n))

  lazy val all: Seq[Selector] = load()

  private def load(): Seq[Selector] =
    XmlParser.parseCatalog(classOf[Selector], "Selector.xml", "Selector", codec)
      .fold(error => throw error, identity)
