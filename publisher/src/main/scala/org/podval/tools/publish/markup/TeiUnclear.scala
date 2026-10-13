package org.podval.tools.publish.markup

import org.podval.tools.publish.site.PageErrorReporter
import org.podval.xml.{Xml, XmlAttribute}
import scala.collection.mutable

/** TEI `<unclear cert reason>` → `data-cert` and a hover tip. `@hand` is applied on the host first. */
object TeiUnclear:
  val tip: Tip = Tip("unclear")
  private object Converted extends XmlAttribute("data-unclear-tip")
  private object Cert extends XmlAttribute("data-cert")

  def convert(
    element: Xml.Element,
    hands: Map[String, TeiHand.Hand],
    errorReporter: PageErrorReporter,
    reported: mutable.Set[String]
  ): Xml.Element =
    if !element.isNamed("unclear") || element.get(Converted).isDefined then element
    else
      val cert: Option[String] = element.get("cert").map(_.trim).filter(_.nonEmpty).map(_.toLowerCase)
      val reason: Option[String] = element.get("reason").map(_.trim).filter(_.nonEmpty)
      val marked: Xml.Element = cert.fold(element)(c => element.set(Cert, c))
      val text: Option[String] = tipText(cert, reason)
      // Guard on the host before the hand wrap, so a second visit does not attach a second unclear tip.
      val host: Xml.Element = text.fold(marked)(_ => marked.set(Converted, "true"))
      val handed: Xml.Element = TeiHand.convert(host, hands, errorReporter, reported)
      text.fold(handed)(t => tip.attachTip(handed, Seq(Xml.text(t))))

  private def tipText(cert: Option[String], reason: Option[String]): Option[String] =
    (cert, reason) match
      case (Some(certainty), Some(why)) => Some(s"$certainty — $why")
      case (Some(certainty), None) => Some(certainty)
      case (None, Some(why)) => Some(why)
      case (None, None) => None
