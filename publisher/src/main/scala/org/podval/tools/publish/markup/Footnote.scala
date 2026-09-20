package org.podval.tools.publish.markup

import org.podval.tools.publish.site.{PageError, PageErrorReporter}
import org.podval.xml.{CssClass, Xml, XmlAttribute, XmlElement}

enum FootnoteScope derives CanEqual:
  case Document
  case Table(index: Int)

// Details of the footnote internal representation.
object Footnote:
  private object CorrelationId extends XmlAttribute("footnote-correlation-id")

  private object LinkClass extends CssClass("footnote-link")

  private object BodyClass extends CssClass("footnote")

  private object BackLinkClass extends CssClass("footnote-backlink")

  private object ScopeAttr extends XmlAttribute("data-footnote-scope")

  private val alphabet: String = "abcdefghijklmnopqrstuvwxyz"

  val tip: Tip = Tip("footnote")

  def isLink(element: Xml.Element): Boolean =
    element.has(LinkClass) && element.get(CorrelationId).isDefined

  def isBody(element: Xml.Element): Boolean = element.has(BodyClass)

  def getCorrelationId(element: Xml.Element): String = element.get(CorrelationId).get

  def prefixCorrelation(element: Xml.Element, prefix: String): Xml.Element =
    element.get(CorrelationId).fold(element)(id => element.set(CorrelationId, prefix + id))

  def remapped(footnote: Footnote, correlationId: String, number: Int): Footnote =
    remapped(footnote, correlationId, number, footnote.scope)

  def remapped(
    footnote: Footnote,
    correlationId: String,
    number: Int,
    scope: FootnoteScope
  ): Footnote =
    Footnote(correlationId, number, footnote.nodes, scope)

  def letterLabel(index0: Int): String =
    val q: Int = index0 / 26
    val r: Int = index0 % 26
    val ch: String = alphabet.drop(r).take(1)
    if q == 0 then ch else letterLabel(q - 1) + ch

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
  def finish(
    xml: Xml.Element,
    report: PageErrorReporter,
    localTables: Boolean = false
  ): Xml.Element =
    val (combined: Map[String, Footnote], stripped: Xml.Element) = harvest(xml)
    val treeIds: Seq[String] = linkIds(stripped).distinct
    if combined.isEmpty && treeIds.isEmpty then xml
    else
      reportOrphans(combined, stripped, report)
      val emitted: Map[String, Footnote] = numbered(stripped, combined, report, localTables)
      val withBodies: Xml.Element = appendReferenced(stripped, emitted)
      resolveTree(withBodies, combined, emitted, attachTip = true, report)

  /** Number footnotes in document-link order; strip bodies from the tree and from parent nodes. */
  def harvest(xml: Xml.Element): (Map[String, Footnote], Xml.Element) =
    val numbers: Map[String, Int] = linkIds(xml).distinct.zipWithIndex.toMap
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
    val innerIds: Set[String] =
      combined.values.flatMap(footnote =>
        footnote.nodes.flatMap(node => node.asElement.fold(Seq.empty[String])(linkIds))
      ).toSet
    combined.keys.foreach: id =>
      if !treeIds.contains(id) && !innerIds.contains(id) then
        report.error(PageError.OrphanFootnote, s"orphan footnote '$id'")

  /** Kind from `hostTree` (full authored tree) plus `tree`; emit-time `k` on `tree`. */
  def numbered(
    tree: Xml.Element,
    combined: Map[String, Footnote],
    report: PageErrorReporter,
    localTables: Boolean,
    hostTree: Option[Xml.Element] = None
  ): Map[String, Footnote] =
    val hostKinds: Map[String, Kind] =
      hostTree.fold(Map.empty[String, Kind])(assignScopes(_, localTables, report))
    val kinds: Map[String, Kind] =
      hostKinds ++ assignScopes(tree, localTables, report, skip = hostKinds.keySet)
    remapEmitted(tree, combined, kinds)

  // Add bodies of the footnotes referenced in the selected XML
  // TODO how do multi-level footnotes look?
  def appendReferenced(
    xml: Xml.Element,
    footnotes: Map[String, Footnote]
  ): Xml.Element =
    val wrapped: Xml.Element =
      if footnotes.values.exists(isTableScope) then wrapTables(xml, footnotes)
      else xml
    val toAdd: Seq[Footnote] = linkIds(wrapped).distinct.flatMap(footnotes.get).filter: footnote =>
      footnote.scope == FootnoteScope.Document
    if toAdd.isEmpty then wrapped
    else
      val footnotesDiv: Xml.Element = Xml
        .element(XmlElement.Div)
        .addClass("footnotes")
        .setChildren(toAdd.map(_.body))
      wrapped.setChildren(wrapped.getChildren :+ footnotesDiv)

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

  // gatherWithContext has one context slot; a table inside a tip would hide the tip.
  private enum Kind derives CanEqual:
    case Document
    case Table

  private def assignScopes(
    tree: Xml.Element,
    localTables: Boolean,
    report: PageErrorReporter,
    skip: Set[String] = Set.empty
  ): Map[String, Kind] =
    if !localTables then
      linkIds(tree).distinct.filterNot(skip.contains).map(_ -> Kind.Document).toMap
    else
      val occurrences: Seq[(String, Option[Xml.Element])] =
        collectOccurrences(tree).filter((id, _) => !skip.contains(id))
      combine(occurrences, report)

  /** `None` is running text; `Some(table)` is that layout table (identity). */
  private def collectOccurrences(element: Xml.Element): Seq[(String, Option[Xml.Element])] =
    def loop(
      el: Xml.Element,
      insideTip: Boolean,
      nearest: Option[Xml.Element]
    ): Seq[(String, Option[Xml.Element])] =
      if isCode(el) then Seq.empty
      else
        val nextTip: Boolean = insideTip || isTipClass(el)
        val nextNearest: Option[Xml.Element] =
          if nextTip then nearest
          else if isLayoutTable(el) then Some(el)
          else nearest
        val here: Seq[(String, Option[Xml.Element])] =
          if !isLink(el) then Seq.empty
          else
            val loc: Option[Xml.Element] = if nextTip then None else nextNearest
            Seq(getCorrelationId(el) -> loc)
        here ++ el.flatMapElements(loop(_, nextTip, nextNearest))
    loop(element, insideTip = false, nearest = None)

  private def combine(
    occurrences: Seq[(String, Option[Xml.Element])],
    report: PageErrorReporter
  ): Map[String, Kind] =
    val grouped: Map[String, Seq[Option[Xml.Element]]] =
      occurrences.foldLeft(Map.empty[String, Seq[Option[Xml.Element]]]):
        case (acc, (id, loc)) => acc.updated(id, acc.getOrElse(id, Seq.empty) :+ loc)
    occurrences.map(_._1).distinct.map: id =>
      val locs: Seq[Option[Xml.Element]] = grouped.getOrElse(id, Seq.empty)
      val tables: Seq[Xml.Element] = uniqueEq(locs.flatten)
      val hasDocument: Boolean = locs.exists(_.isEmpty)
      val kind: Kind =
        if tables.isEmpty then Kind.Document
        else if !hasDocument && tables.size == 1 then Kind.Table
        else
          report.error(PageError.FootnoteScopeConflict, s"footnote '$id' has conflicting scopes")
          Kind.Document
      id -> kind
    .toMap

  private def remapEmitted(
    tree: Xml.Element,
    combined: Map[String, Footnote],
    kinds: Map[String, Kind]
  ): Map[String, Footnote] =
    val occurrences: Seq[(String, Option[Xml.Element])] = collectOccurrences(tree)
    val documentIds: Seq[String] =
      occurrences.map(_._1).distinct.filter(id => kinds.get(id).contains(Kind.Document))
    val tableOccs: Seq[(String, Xml.Element)] = occurrences.flatMap: (id, loc) =>
      if kinds.get(id).contains(Kind.Table) then loc.map(id -> _) else None
    val tables: Seq[Xml.Element] = uniqueEq(tableOccs.map(_._2))
    val documentEmitted: Seq[(String, Footnote)] =
      documentIds.zipWithIndex.flatMap: (id, index) =>
        combined.get(id).map(footnote => id -> remapped(footnote, id, index + 1, FootnoteScope.Document))
    val tableEmitted: Seq[(String, Footnote)] =
      tables.zipWithIndex.flatMap: (table, index) =>
        val k: Int = index + 1
        val ids: Seq[String] = tableOccs.filter((_, t) => t eq table).map(_._1).distinct
        ids.zipWithIndex.flatMap: (id, n) =>
          combined.get(id).map(footnote =>
            id -> remapped(footnote, id, n + 1, FootnoteScope.Table(k))
          )
    (documentEmitted ++ tableEmitted).toMap

  private def wrapTables(
    element: Xml.Element,
    footnotes: Map[String, Footnote],
    insideTip: Boolean = false
  ): Xml.Element =
    if isCode(element) then element
    else
      val nextTip: Boolean = insideTip || isTipClass(element)
      val withChildren: Xml.Element = element.setChildren(
        element.getChildren.map: child =>
          child.asElement.fold(child)(wrapTables(_, footnotes, nextTip))
      )
      if nextTip || !isLayoutTable(withChildren) then withChildren
      else
        val ids: Seq[String] = collectOccurrences(withChildren)
          .flatMap: (id, loc) =>
            Option.when(loc.exists(_ eq withChildren))(id)
          .distinct
          .filter(id => footnotes.get(id).exists(isTableScope))
        if ids.isEmpty then withChildren
        else
          val notes: Seq[Footnote] = ids.flatMap(footnotes.get)
          val list: Xml.Element = Xml
            .element(XmlElement.Div)
            .addClass("footnotes")
            .addClass("table-footnotes")
            .setChildren(notes.map(_.body))
          Xml
            .element(XmlElement.Div)
            .addClass("table-with-notes")
            .setChildren(Seq(withChildren: Xml.Node, list: Xml.Node))

  private def isLayoutTable(element: Xml.Element): Boolean =
    element.isElement(XmlElement.Table) &&
      !element.hasClass("collection-index") &&
      !element.hasClass("document-header")

  private def isTipClass(element: Xml.Element): Boolean =
    element.getClasses.exists(_.endsWith("-tip"))

  private def isCode(element: Xml.Element): Boolean =
    element.isNamed(XmlElement.Code.localName)

  private def uniqueEq(tables: Seq[Xml.Element]): Seq[Xml.Element] =
    tables.foldLeft(Seq.empty[Xml.Element]): (acc, table) =>
      if acc.exists(_ eq table) then acc else acc :+ table

  private def isTableScope(footnote: Footnote): Boolean =
    footnote.scope match
      case FootnoteScope.Table(_) => true
      case _ => false

  private def stripInnerBodies(nodes: Xml.Nodes): Xml.Nodes =
    nodes.flatMapNodes: node =>
      node.asElement match
        case Some(el) if isBody(el) => Seq.empty
        case Some(el) => Seq(el.setChildren(stripInnerBodies(el.getChildren)))
        case None => Seq(node)

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
  val nodes: Xml.Nodes,
  val scope: FootnoteScope = FootnoteScope.Document
):
  def label: String = scope match
    case FootnoteScope.Document => number.toString
    case FootnoteScope.Table(_) => Footnote.letterLabel(number - 1)

  private def linkId: String = scope match
    case FootnoteScope.Document => s"_footnote_src_$number"
    case FootnoteScope.Table(k) => s"_table_${k}_fn_src_$label"

  private def bodyId: String = scope match
    case FootnoteScope.Document => s"_footnote_$number"
    case FootnoteScope.Table(k) => s"_table_${k}_fn_$label"

  private def scopeName: Option[String] = scope match
    case FootnoteScope.Document => None
    case FootnoteScope.Table(_) => Some("table")

  def link: Xml.Element =
    val result: Xml.Element = Xml
      .element(XmlElement.A)
      .add(Footnote.LinkClass)
      .setId(linkId)
      .setHref(s"#$bodyId")
      .setText(label)
    scopeName.fold(result)(result.set(Footnote.ScopeAttr, _))

  def body: Xml.Element =
    val result: Xml.Element = Xml
      .element(XmlElement.Span)
      .add(Footnote.BodyClass)
      .setId(bodyId)
      .setChildren(backLink +: nodes)
    scopeName.fold(result)(result.set(Footnote.ScopeAttr, _))

  private def backLink: Xml.Element = Xml
    .element(XmlElement.A)
    .add(Footnote.BackLinkClass)
    .setHref(s"#$linkId")
    .setText(label)
