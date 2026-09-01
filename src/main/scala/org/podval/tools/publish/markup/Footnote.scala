package org.podval.tools.publish.markup

import org.podval.xml.{HtmlClass, Xml, XmlAttribute, XmlElement, XmlUtil}

// Details of the footnote internal representation.
object Footnote:
  private object CorrelationId extends XmlAttribute("footnote-correlation-id")

  private object LinkClass extends HtmlClass("footnote-link")

  private object BodyClass extends HtmlClass("footnote")

  private object BackLinkClass extends HtmlClass("footnote-backlink")

  val tip: Tip = Tip("footnote")

  def isLink(element: Xml.Element): Boolean =
    element.has(LinkClass) && element.get(CorrelationId).isDefined

  def isBody(element: Xml.Element): Boolean = element.has(BodyClass)

  def getCorrelationId(element: Xml.Element): String = element.get(CorrelationId).get

  // Note: footnote link will end up as an <a>, but the stub is not -
  // to avoid it being assigned an id and getting resolved ;)
  def link(correlationId: String): Xml.Element = Xml
    .element("span")
    .add(LinkClass)
    .set(CorrelationId, correlationId)
  
  def body(correlationId: String, content: Xml.Nodes): Xml.Element = Xml
    .element("span")
    .add(BodyClass)
    .set(CorrelationId, correlationId)
    .setChildren(content)

  def linkIds(xml: Xml.Element): Seq[String] =
    xml.gather(element =>
      Option.when(isLink(element))(getCorrelationId(element))
    )

  /** Replace leftover containers (caller says which) with the IR bodies inside them. */
  def unwrapLeftovers(xml: Xml.Element, isContainer: Xml.Element => Boolean): Xml.Element =
    xml.transform(element =>
      element.setChildren(XmlUtil.convertElements(element.getChildren, leftover =>
        Option.when(isContainer(leftover))(
          leftover.gather(el => Option.when(isBody(el))(el: Xml.Node))
        )
      ))
    )

  /** Harvest bodies, append the list while stubs still have ids, then number the links. */
  def finish(xml: Xml.Element): Xml.Element =
    val (footnotes: Map[String, Footnote], stripped: Xml.Element) = harvest(xml)
    if footnotes.isEmpty then xml
    else
      val withBodies: Xml.Element = appendReferenced(stripped, footnotes)
      withBodies.transform(
        element => resolveLink(element, footnotes, attachTip = true),
        stopAtCode = false
      )

  /** Number footnotes in document-link order, then drop bodies from the tree. */
  def harvest(xml: Xml.Element): (Map[String, Footnote], Xml.Element) =
    val numbers: Map[String, Int] = linkIds(xml).zipWithIndex.toMap
    val footnotes: Map[String, Footnote] = xml
      .gather(element =>
        Option.when(isBody(element)):
          val correlationId: String = element.get(CorrelationId).getOrElse:
            throw IllegalStateException(
              s"footnote body without correlation id: <${element.getName} class=${element.getClasses.mkString(" ")}>"
            )
          correlationId -> Footnote(
            correlationId = correlationId,
            number = numbers(correlationId) + 1,
            nodes = element.getChildren
          )
      )
      .toMap
    val stripped: Xml.Element = xml.transform(element =>
      element.setChildren(element
        .getChildren
        .filterNot(_.asElement.exists(isBody))
      )
    )
    (footnotes, stripped)

  // Add bodies of the footnotes referenced in the selected XML
  // TODO footnotes placed at the end of elements like table, not the overall end?
  // TODO how do multi-level footnotes look?
  def appendReferenced(
    xml: Xml.Element,
    footnotes: Map[String, Footnote]
  ): Xml.Element =
    val toAdd: Seq[Footnote] = linkIds(xml).map(footnotes)
    if toAdd.isEmpty then xml
    else
      val footnotesDiv: Xml.Element = Xml
        .element("div")
        .addClass("footnotes")
        .setChildren(toAdd.map(_.body))
      xml.setChildren(xml.getChildren :+ footnotesDiv)

  def resolveLink(
    element: Xml.Element,
    footnotes: Map[String, Footnote],
    attachTip: Boolean
  ): Xml.Element =
    if !isLink(element) then element
    else
      val footnote: Footnote = footnotes(getCorrelationId(element))
      var result: Xml.Element = footnote.link
      if attachTip then
        val content: Xml.Nodes = footnote.nodes.filterNot(_.isWhitespace)
        if content.nonEmpty then
          result = tip.attachTip(result, content)
      result

final class Footnote(
  val correlationId: String,
  val number: Int,
  val nodes: Xml.Nodes
):
  private def linkId: String = s"_footnote_src_$number"
  private def bodyId: String = s"_footnote_$number"

  def link: Xml.Element = Xml
    .element(XmlElement.A)
    .add(Footnote.LinkClass)
    .setId(linkId)
    .setHref(s"#$bodyId")
    .setText(number.toString)

  def body: Xml.Element = Xml
    .element("span")
    .add(Footnote.BodyClass)
    .setId(bodyId)
    .setChildren(backLink +: nodes)

  private def backLink: Xml.Element = Xml
    .element(XmlElement.A)
    .add(Footnote.BackLinkClass)
    .setHref(s"#$linkId")
    .setText(number.toString)
