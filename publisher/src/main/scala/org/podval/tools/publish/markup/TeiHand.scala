package org.podval.tools.publish.markup

import org.podval.tools.publish.site.{PageError, PageErrorReporter}
import org.podval.xml.{Xml, XmlAttribute}
import scala.collection.mutable

/** TEI `@hand="#id"` → `data-medium` and a hover tip from the `handNote` text. */
object TeiHand:
  val tip: Tip = Tip("hand")
  private object Converted extends XmlAttribute("data-hand-tip")
  private object Medium extends XmlAttribute("data-medium")

  final case class Hand(medium: Option[String], text: String)

  def collect(root: Xml.Element): Map[String, Hand] =
    DialectWalk.gather(root)(el => Option.when(el.isNamed("handNote"))(el))
      .foldLeft(Map.empty[String, Hand]): (found, note) =>
        note.get(XmlAttribute.XmlId).map(_.trim).filter(_.nonEmpty) match
          case Some(id) if !found.contains(id) =>
            found.updated(id, Hand(
              medium = note.get("medium").map(_.trim).filter(_.nonEmpty),
              text = note.getText.trim
            ))
          case _ =>
            found

  def convert(
    element: Xml.Element,
    hands: Map[String, Hand],
    errorReporter: PageErrorReporter,
    reported: mutable.Set[String]
  ): Xml.Element =
    if element.get(Converted).isDefined then element
    else element.get("hand").map(_.trim).filter(_.nonEmpty).fold(element): pointer =>
      pointerId(pointer).flatMap(hands.get) match
        case Some(hand) if hand.text.nonEmpty || hand.medium.nonEmpty =>
          val withMedium: Xml.Element = hand.medium match
            case Some(medium) => element.set(Medium, medium)
            case None => element
          if hand.text.isEmpty then withMedium
          else tip.attachTip(withMedium.set(Converted, "true"), Seq(Xml.text(hand.text)))
        case Some(_) =>
          element
        case None =>
          if reported.add(pointer) then errorReporter.error(PageError.UnresolvedHand, pointer)
          element

  // One same-document pointer (`#` + id). Anything else is unresolved.
  private def pointerId(pointer: String): Option[String] =
    val id: String = pointer.drop(1)
    Option.when(pointer.startsWith("#") && id.nonEmpty && !id.exists(ch => ch.isWhitespace || ch == '#'))(id)
