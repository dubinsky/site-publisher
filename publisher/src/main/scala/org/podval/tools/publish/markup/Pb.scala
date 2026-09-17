package org.podval.tools.publish.markup

import org.podval.tools.publish.util.Icon
import org.podval.xml.{CssClass, Xml, XmlAttribute, XmlElement}

/** TEI `pb` harvested from the raw tree for collection `Страницы`, missing-photo notes, and facsimile JPEGs. */
final class Pb(
  //  val id: Option[String],
  val n: String,
  val isMissing: Boolean,
  val isEmpty: Boolean,
  val facs: Option[String] = None
)

object Pb:
  object Class extends CssClass("pb")

  def pageId(n: String): String = s"p$n"

  def is(element: Xml.Element): Boolean = element.isA && element.has(Class)

  def anchor(n: Option[String]): Xml.Element =
    var result: Xml.Element = Xml.element(XmlElement.A)
      .add(Class)
      .set(XmlAttribute.Title, "Facsimile")
      .set(XmlAttribute.Target, "facsimile")
      .setChildren(Seq(Icon.images.xml: Xml.Node))
    n.foreach: n =>
      result = result
        .setId(pageId(n))
        .set("aria-label", s"Facsimile page $n")
    result

  def harvest(xml: Xml.Element): Seq[Pb] =
    xml.gather(
      el =>
        if !el.isNamed("pb") then None
        else el.get("n").map(_.trim).filter(_.nonEmpty).map: n =>
          Pb(
            n,
            isTrue(el.get("missing")),
            isTrue(el.get("empty")),
            facs = el.get("facs").map(_.trim).filter(_.nonEmpty)
          ),
      stopAtCode = false
    ).toSeq

  private def isTrue(value: Option[String]): Boolean =
    value.exists: v =>
      val t: String = v.trim
      t == "true" || t == "1"
