package org.podval.tools.publish.markup

import org.podval.metadata.Language
import org.podval.tools.publish.site.Messages
import org.podval.tools.publish.util.Icon
import org.podval.xml.{CssClass, Xml, XmlAttribute, XmlElement}

/** TEI `pb` harvested from the raw tree for collection `Страницы`, the missing-leaf line, and facsimile JPEGs. */
final class Pb(
  //  val id: Option[String],
  val n: String,
  val isMissing: Boolean,
  val isEmpty: Boolean,
  val facs: Option[String] = None
)

object Pb:
  object Class extends CssClass("pb")
  private object FaceClass extends CssClass("pb-face")
  private object NameClass extends CssClass("pb-name")
  private object LabelClass extends CssClass("pb-label")
  private object TipClass extends CssClass("pb-tip")

  def pageId(n: String): String = s"p$n"

  def is(element: Xml.Element): Boolean = element.isA && element.has(Class)

  /** A page break in the transcription: the facsimile anchor, or the missing-leaf line. */
  def isBreak(element: Xml.Element): Boolean =
    element.has(Class) && (element.isA || element.isElement(XmlElement.Span))

  /** One line. Not a link, and not a screen tall. Faded icon, then the missing-photograph sentence. */
  def missingLine(
    n: Option[String],
    empty: Boolean,
    chrome: Option[(String, String)] = None
  )(using Language.Spec): Xml.Element =
    val label: String = displayLabel(n)
    val sentence: String = missingSentence(label, empty)
    // The sentence is the visible text, so the tooltip is not also the accessible description.
    var result: Xml.Element = Xml.element(XmlElement.Span)
      .add(Class)
      .setChildren(Seq(face(Seq(icon, textSpan(LabelClass, sentence)), sentence, n)))
    n.foreach(value => result = result.setId(pageId(value)))
    result = result.set("data-facsimile-missing", "true")
    if empty then result = result.set("data-facsimile-empty", "true")
    withChrome(result, chrome)

  def anchor(
    n: Option[String],
    blank: Boolean = false,
    chrome: Option[(String, String)] = None
  )(using Language.Spec): Xml.Element =
    val label: String = displayLabel(n)
    val sentence: String = facsimileSentence(label)
    val visible: Seq[Xml.Node] =
      if label.isEmpty then Seq(icon) else Seq(icon, textSpan(NameClass, label))
    var result: Xml.Element = Xml.element(XmlElement.A)
      .add(Class)
      .set(XmlAttribute.Target, "facsimile")
      .setChildren(Seq(face(visible, sentence, n)))
    n.foreach: value =>
      result = result
        .setId(pageId(value))
        .set("aria-describedby", tipId(value))
    if blank then result = result.set("data-facsimile-blank", "true")
    withChrome(result, chrome)

  private def withChrome(element: Xml.Element, chrome: Option[(String, String)]): Xml.Element =
    chrome.fold(element): pair =>
      val (code, dir) = pair
      element.set(XmlAttribute.Lang, code).set("dir", dir)

  private def displayLabel(n: Option[String]): String =
    n.fold("")(PageType.Manuscript.displayName)

  private def missingSentence(label: String, empty: Boolean)(using Language.Spec): String =
    if label.isEmpty then Messages.text("pb.missing")
    else if empty then Messages.text("pb.missingEmpty", "label" -> label)
    else Messages.text("pb.missingNamed", "label" -> label)

  private def facsimileSentence(label: String)(using Language.Spec): String =
    if label.isEmpty then Messages.text("pb.facsimile")
    else Messages.text("pb.facsimileNamed", "label" -> label)

  private def tipId(n: String): String = s"${pageId(n)}-tip"

  private def icon: Xml.Node = Icon.images.html

  private def textSpan(css: CssClass, text: String): Xml.Node =
    Xml.element(XmlElement.Span).add(css).setText(text)

  /** Tip is inside the face, not the page break. A blank scan stretches the anchor to the screen. */
  private def face(visible: Seq[Xml.Node], sentence: String, n: Option[String]): Xml.Node =
    var tip: Xml.Element = Xml.element(XmlElement.Span)
      .add(TipClass)
      .set(XmlAttribute.Role, "tooltip")
      .setText(sentence)
    n.foreach(value => tip = tip.setId(tipId(value)))
    Xml.element(XmlElement.Span).add(FaceClass).setChildren(visible :+ tip)

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
