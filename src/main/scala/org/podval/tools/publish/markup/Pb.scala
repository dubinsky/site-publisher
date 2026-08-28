package org.podval.tools.publish.markup

import org.podval.xml.Xml

/** TEI `pb` harvested from the raw tree for collection `Страницы` and missing-photo notes. */
final class Pb(
  val n: String,
  val isMissing: Boolean,
  val isEmpty: Boolean
)

object Pb:
  def pageId(n: String): String = s"p$n"

  def harvest(xml: Xml.Element): Seq[Pb] =
    xml.gather(
      el =>
        if el.localName != "pb" then None
        else el.get("n").map(_.trim).filter(_.nonEmpty).map: n =>
          Pb(n, isTrue(el.get("missing")), isTrue(el.get("empty"))),
      stopAtCode = false
    ).toSeq

  private def isTrue(value: Option[String]): Boolean =
    value.exists: v =>
      val t: String = v.trim
      t == "true" || t == "1"
