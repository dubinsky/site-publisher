package org.podval.tools.publish.markup

import org.podval.xml.Xml

/** A collection `<part from="000">` title row. `from` is the first original document's file name. */
final class CollectionPart(
  val n: Option[String],
  val from: String,
  val title: Option[Xml.Element]
)

object CollectionPart:
  def harvest(xml: Xml.Element): Seq[CollectionPart] =
    xml.getChildren.flatMap(_.asElement).filter(_.localName == "part").flatMap(parse).toSeq

  private def parse(element: Xml.Element): Option[CollectionPart] =
    element.get("from").map(_.trim).filter(_.nonEmpty).map: from =>
      CollectionPart(
        n = element.get("n").map(_.trim).filter(_.nonEmpty),
        from = from,
        title = element.getChildren.flatMap(_.asElement).find(_.localName == "title")
      )
