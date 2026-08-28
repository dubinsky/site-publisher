package org.podval.tools.publish.markup

import org.podval.xml.Xml

/** TEI `entityLists` directory index specs: kind + role buckets.
  * Harvested from the raw tree; member lists are generated in `page.EntityLists`. */
object EntityLists:
  final class Index(val lists: Seq[Spec])

  final class Spec(
    val kind: EntityKind,
    val id: String,
    val role: Option[String],
    val title: String
  )

  def harvest(xml: Xml.Element): Option[Index] =
    Option.when(xml.localName == "entityLists"):
      Index(
        xml.getChildren.flatMap(_.asElement).flatMap(parseList).toSeq
      )

  private def parseList(element: Xml.Element): Option[Spec] =
    for
      kind <- EntityKind.values.find(_.listElement == element.localName)
      id <- element.get("n").map(_.trim).filter(_.nonEmpty)
      title <- listTitle(element)
    yield Spec(
      kind = kind,
      id = id,
      role = element.get("role").map(_.trim).filter(_.nonEmpty),
      title = title
    )

  private def listTitle(element: Xml.Element): Option[String] =
    element.getChildren.flatMap(_.asElement)
      .find(el =>
        val name: String = el.localName
        name == "title" || name == "tei-title"
      )
      .map(_.getText.trim)
      .filter(_.nonEmpty)
