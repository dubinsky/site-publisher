package org.podval.tools.publish.markup

import org.podval.tools.publish.util.Files
import org.podval.xml.{Xml, XmlParser}

/** A store/collection `by/@selector` (or a document facet). Loaded from `Selector.xml`
  * (same catalog as the old Open Torah collector). Display name prefers Russian. */
final class Selector(
  val names: Seq[Selector.Name],
  val title: Option[String]
):
  def displayName: String =
    names.find(_.lang.contains("ru")).orElse(names.headOption).map(_.n).getOrElse("")

  def matches(n: String): Boolean =
    names.exists(_.n.equalsIgnoreCase(n))

object Selector:
  final class Name(
    val n: String,
    val lang: Option[String]
  )

  def displayName(n: String): String = find(n).map(_.displayName).getOrElse(n)

  def find(n: String): Option[Selector] = all.find(_.matches(n))

  lazy val all: Seq[Selector] = load()

  private def load(): Seq[Selector] =
    val xml: Xml.Element = XmlParser.parseXml(
      Files.readResource("/org/podval/tools/publish/markup/Selector.xml")
    ) match
      case Right(root) => root
      case Left(error) => throw error
    xml.getChildren.flatMap(_.asElement).filter(_.localName == "selector").flatMap(parse)

  private def parse(element: Xml.Element): Option[Selector] =
    val names: Seq[Name] =
      element.getChildren.flatMap(_.asElement).filter(_.localName == "name").flatMap: el =>
        el.get("n").map(_.trim).filter(_.nonEmpty).map: n =>
          Name(n, el.get("lang").map(_.trim).filter(_.nonEmpty))
    Option.when(names.nonEmpty)(
      Selector(names, element.get("title").map(_.trim).filter(_.nonEmpty))
    )
