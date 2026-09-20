package org.podval.tools.publish.markup

import org.podval.tools.publish.site.{PageError, PageErrorReporter}
import org.podval.xml.{CssClass, Xml, XmlAttribute, XmlElement}

// Details of the footnote internal representation.
object Footnote:
  private object CorrelationId extends XmlAttribute("footnote-correlation-id")

  private object LinkClass extends CssClass("footnote-link")

  private object BodyClass extends CssClass("footnote")

  private object BackLinkClass extends CssClass("footnote-backlink")

  val tip: Tip = Tip("footnote")

  def isLink(element: Xml.Element): Boolean =
    element.has(LinkClass) && element.get(CorrelationId).isDefined

  def isBody(element: Xml.Element): Boolean = element.has(BodyClass)

  def getCorrelationId(element: Xml.Element): String = element.get(CorrelationId).get

  def prefixCorrelation(element: Xml.Element, prefix: String): Xml.Element =
    element.get(CorrelationId).fold(element)(id => element.set(CorrelationId, prefix + id))

  def remapped(footnote: Footnote, correlationId: String, number: Int): Footnote =
    Footnote(correlationId, number, footnote.nodes)

  def uniqueInOrder(ids: Seq[String]): Seq[String] =
    val (ordered, _) = ids.foldLeft((Seq.empty[String], Set.empty[String])):
      case ((acc, seen), id) if seen.contains(id) => (acc, seen)
      case ((acc, seen), id) => (acc :+ id, seen + id)
    ordered

  // Note: footnote link will end up as an <a>, but the stub is not -
  // to avoid it being assigned an id and getting resolved ;)
  def link(correlationId: String): Xml.Element = Xml
    .element(XmlElement.Span)
    .add(LinkClass)
    .set(CorrelationId, correlationId)
  
  def body(correlationId: String, content: Xml.Nodes): Xml.Element = Xml
    .element(XmlElement.Span)
    .add(BodyClass)
    .set(CorrelationId, correlationId)
    .setChildren(content)

  def linkIds(xml: Xml.Element): Seq[String] = xml.gather(element =>
    Option.when(isLink(element))(getCorrelationId(element))
  )

  /** Replace leftover containers (caller says which) with the IR bodies inside them. */
  def unwrapLeftovers(xml: Xml.Element, isContainer: Xml.Element => Boolean): Xml.Element =
    xml.transform(element =>
      element.setChildren(element.getChildren.convertElements(leftover =>
        Option.when(isContainer(leftover))(
          leftover.gather(el => Option.when(isBody(el))(el: Xml.Node))
        )
      ))
    )

  /** Harvest bodies, append the list while stubs still have ids, then number the links. */
  def finish(xml: Xml.Element, report: PageErrorReporter): Xml.Element =
    val (combined: Map[String, Footnote], stripped: Xml.Element) = harvest(xml)
    val treeIds: Seq[String] = uniqueInOrder(linkIds(stripped))
    if combined.isEmpty && treeIds.isEmpty then xml
    else
      reportOrphans(combined, stripped, report)
      val emitted: Map[String, Footnote] = treeIds.zipWithIndex.flatMap:
        (id, index) => combined.get(id).map(footnote => id -> remapped(footnote, id, index + 1))
      .toMap
      val withBodies: Xml.Element = appendReferenced(stripped, emitted)
      resolveTree(withBodies, combined, emitted, attachTip = true, report)

  /** Number footnotes in document-link order; strip bodies from the tree and from parent nodes. */
  def harvest(xml: Xml.Element): (Map[String, Footnote], Xml.Element) =
    val numbers: Map[String, Int] = uniqueInOrder(linkIds(xml)).zipWithIndex.toMap
    val footnotes: Map[String, Footnote] = xml
      .gather(element =>
        Option.when(isBody(element)):
          val correlationId: String = element.get(CorrelationId).getOrElse:
            throw IllegalStateException(
              s"footnote body without correlation id: <${element.getName.qName} class=${element.getClasses.mkString(" ")}>"
            )
          correlationId -> Footnote(
            correlationId = correlationId,
            number = numbers.get(correlationId).fold(0)(_ + 1),
            nodes = stripInnerBodies(element.getChildren)
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

  def reportOrphans(
    combined: Map[String, Footnote],
    tree: Xml.Element,
    report: PageErrorReporter
  ): Unit =
    val treeIds: Set[String] = linkIds(tree).toSet
    val innerIds: Set[String] = combined.values.flatMap(footnote => linkIdsIn(footnote.nodes)).toSet
    combined.keys.foreach: id =>
      if !treeIds.contains(id) && !innerIds.contains(id) then
        report.error(PageError.OrphanFootnote, s"orphan footnote '$id'")

  // Add bodies of the footnotes referenced in the selected XML
  // TODO footnotes placed at the end of elements like table, not the overall end?
  // TODO how do multi-level footnotes look?
  def appendReferenced(
    xml: Xml.Element,
    footnotes: Map[String, Footnote]
  ): Xml.Element =
    val toAdd: Seq[Footnote] = uniqueInOrder(linkIds(xml)).flatMap(footnotes.get)
    if toAdd.isEmpty then xml
    else
      val footnotesDiv: Xml.Element = Xml
        .element(XmlElement.Div)
        .addClass("footnotes")
        .setChildren(toAdd.map(_.body))
      xml.setChildren(xml.getChildren :+ footnotesDiv)

  def resolveLink(
    element: Xml.Element,
    combined: Map[String, Footnote],
    emitted: Map[String, Footnote],
    attachTip: Boolean,
    report: PageErrorReporter
  ): Xml.Element =
    if !isLink(element) then element
    else
      val id: String = getCorrelationId(element)
      combined.get(id) match
        case None =>
          report.error(PageError.UnknownFootnote, s"unknown footnote '$id'")
          element.add(Link.UnresolvedLinkClass)
        case Some(_) =>
          emitted.get(id) match
            case None => element
            case Some(footnote) =>
              var result: Xml.Element = footnote.link
              if attachTip then
                val content: Xml.Nodes = footnote.nodes.filterNot(_.isWhitespace)
                if content.nonEmpty then
                  result = tip.attachTip(result, content)
              result

  private def stripInnerBodies(nodes: Xml.Nodes): Xml.Nodes =
    nodes.flatMapNodes: node =>
      node.asElement match
        case Some(el) if isBody(el) => Seq.empty
        case Some(el) => Seq(el.setChildren(stripInnerBodies(el.getChildren)))
        case None => Seq(node)

  private def linkIdsIn(nodes: Xml.Nodes): Seq[String] =
    nodes.flatMap: node =>
      node.asElement.fold(Seq.empty[String])(linkIds)

  private def resolveTree(
    element: Xml.Element,
    combined: Map[String, Footnote],
    emitted: Map[String, Footnote],
    attachTip: Boolean,
    report: PageErrorReporter
  ): Xml.Element =
    val resolved: Xml.Element = resolveLink(element, combined, emitted, attachTip, report)
    resolved.setChildren(resolved.getChildren.map: child =>
      child.asElement.fold(child): el =>
        resolveTree(
          el,
          combined,
          emitted,
          attachTip && !el.getClasses.exists(_.endsWith("-tip")),
          report
        )
    )

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
    .element(XmlElement.Span)
    .add(Footnote.BodyClass)
    .setId(bodyId)
    .setChildren(backLink +: nodes)

  private def backLink: Xml.Element = Xml
    .element(XmlElement.A)
    .add(Footnote.BackLinkClass)
    .setHref(s"#$linkId")
    .setText(number.toString)
