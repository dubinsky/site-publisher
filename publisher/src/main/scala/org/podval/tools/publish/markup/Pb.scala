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

  def anchor(n: Option[String], blank: Boolean = false): Xml.Element =
    var result: Xml.Element = Xml.element(XmlElement.A)
      .add(Class)
      .set(XmlAttribute.Title, "Facsimile")
      .set(XmlAttribute.Target, "facsimile")
      .setChildren(Seq(Icon.images.html: Xml.Node))
    n.foreach: n =>
      result = result
        .setId(pageId(n))
        .set("aria-label", s"Facsimile page $n")
    if blank then result = result.set("data-facsimile-blank", "true")
    result

  /** Non-missing `pb/@n` with no text before the next `pb`. The aside gives that page a screen of height. */
  def noTranscription(xml: Xml.Element): Set[String] =
    fold(Reading(Set.empty, None, false), xml).close

  private final case class Reading(blank: Set[String], open: Option[String], seen: Boolean):
    def close: Set[String] =
      open.filter(_ => !seen).fold(blank)(blank + _)

    def see: Reading =
      if seen then this else copy(seen = true)

    def break(n: Option[String], missing: Boolean): Reading =
      val closed: Set[String] = open.filter(_ => !seen).fold(blank)(blank + _)
      val next: Option[String] = if missing then None else n
      Reading(closed, next, false)

  private def fold(reading: Reading, node: Xml.Node): Reading =
    node.asElement match
      case Some(el) if el.isNamed("teiHeader") => reading
      case Some(el) if el.isNamed("pb") =>
        val n: Option[String] = el.get("n").map(_.trim).filter(_.nonEmpty)
        reading.break(n, isTrue(el.get("missing")))
      case Some(el) =>
        el.getChildren.foldLeft(reading)(fold)
      case None if node.isCharacters =>
        reading.see
      case None =>
        reading

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
