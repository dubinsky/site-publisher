package org.podval.tools.publish.markup

import org.podval.tools.publish.site.{PageError, PageErrorReporter}
import org.podval.xml.{CssClass, Xml, XmlAttribute, XmlElement}

enum FootnoteScope derives CanEqual:
  case Document
  case Table(index: Int)
  case Nested(parentId: String)

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

  def prefixCorrelationTree(el: Xml.Element, prefix: String): Xml.Element =
    el.transform(prefixCorrelation(_, prefix), stopAtCode = false)

  def remapped(
    footnote: Footnote,
    correlationId: String,
    number: Int,
    scope: FootnoteScope,
    nodes: Xml.Nodes,
    parentBodyId: Option[String] = None
  ): Footnote =
    Footnote(correlationId, number, nodes, scope, parentBodyId)

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

  def linkIds(nodes: Xml.Nodes): Seq[String] =
    nodes.flatMap(node => node.asElement.fold(Seq.empty[String])(linkIds))

  /** Replace leftover containers (caller says which) with the IR bodies inside them. */
  def unwrapLeftovers(xml: Xml.Element, isContainer: Xml.Element => Boolean): Xml.Element =
    xml.transform(element =>
      element.setChildren(Xml.convertElements(element.getChildren)(leftover =>
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
    if combined.isEmpty && linkIds(stripped).isEmpty then xml
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
      combined.values.flatMap(footnote => linkIds(footnote.nodes)).toSet
    combined.keys.foreach: id =>
      if !treeIds.contains(id) && !innerIds.contains(id) then
        report.error(PageError.OrphanFootnote, s"orphan footnote '$id'")

  /** Kind from `hostTree` (full authored tree) plus `tree`; emit-time `k` on `tree`. */
  def numbered(
    tree: Xml.Element,
    combined: Map[String, Footnote],
    report: PageErrorReporter,
    localTables: Boolean,
    hostTree: Option[Xml.Element] = None,
    hostFootnotes: Map[String, Footnote] = Map.empty,
    emitLeftovers: Boolean = true
  ): Map[String, Footnote] =
    val hostAssigned: Assigned =
      hostTree.fold(Assigned.empty)(assignScopes(_, hostFootnotes, localTables, report))
    val treeAssigned: Assigned =
      assignScopes(tree, combined, localTables, report, skip = hostAssigned.kinds.keySet)
    val kinds: Map[String, Kind] = hostAssigned.kinds ++ treeAssigned.kinds
    val cycleIds: Set[String] = hostAssigned.cycleIds ++ treeAssigned.cycleIds
    remapEmitted(
      tree,
      combined,
      kinds,
      cycleIds,
      hostTreeIds = hostTree.fold(Set.empty[String])(linkIds(_).toSet),
      emitLeftovers = emitLeftovers
    )

  // Add bodies of the footnotes referenced in the selected XML
  def appendReferenced(
    xml: Xml.Element,
    footnotes: Map[String, Footnote],
    emitLeftovers: Boolean = true
  ): Xml.Element =
    val wrapped: Xml.Element =
      if footnotes.values.exists(isTableScope) then wrapTables(xml, footnotes)
      else xml
    val treeIds: Seq[String] = linkIds(wrapped).distinct
    val treeDocs: Seq[Footnote] = treeIds.flatMap(footnotes.get).filter(isDocumentScope)
    // Cycle/conflict leftovers have no tree marker; chunks omit them so they are not repeated.
    val leftoverDocs: Seq[Footnote] =
      if !emitLeftovers then Seq.empty
      else
        val mentioned: Set[String] = footnotes.values.flatMap(footnote => linkIds(footnote.nodes)).toSet
        footnotes.values
          .filter: footnote =>
            isDocumentScope(footnote) &&
              !treeIds.contains(footnote.correlationId) &&
              mentioned.contains(footnote.correlationId)
          .toSeq
          .sortBy(_.number)
    val toAdd: Seq[Footnote] = treeDocs ++ leftoverDocs
    if toAdd.isEmpty then wrapped
    else
      val footnotesDiv: Xml.Element = Xml
        .element(XmlElement.Div)
        .addClass("footnotes")
        .setChildren(toAdd.map(emitBody(_, footnotes)))
      wrapped.setChildren(wrapped.getChildren :+ footnotesDiv)

  def resolveLink(
    element: Xml.Element,
    combined: Map[String, Footnote],
    emitted: Map[String, Footnote],
    attachTip: Boolean,
    report: PageErrorReporter
  ): Xml.Element =
    resolveLink(element, combined, emitted, attachTip, report, withId = attachTip)

  def resolveLink(
    element: Xml.Element,
    combined: Map[String, Footnote],
    emitted: Map[String, Footnote],
    attachTip: Boolean,
    report: PageErrorReporter,
    withId: Boolean
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
              // Tooltip copies must not steal src ids; transclusion copies skip tips but still need them.
              val numbered: Xml.Element = footnote.link(withId = withId)
              if !attachTip then numbered
              else
                val content: Xml.Nodes = footnote.nodes.filterNot(_.isWhitespace)
                if content.isEmpty then numbered else tip.attachTip(numbered, content)

  // gatherWithContext has one context slot; a table inside a tip would hide the tip.
  private enum Kind derives CanEqual:
    case Document
    case Table
    case Nested(parentId: String)

  private final class Assigned(
    val kinds: Map[String, Kind],
    val cycleIds: Set[String]
  )

  private object Assigned:
    val empty: Assigned = Assigned(Map.empty, Set.empty)

  private def assignScopes(
    tree: Xml.Element,
    footnotes: Map[String, Footnote],
    localTables: Boolean,
    report: PageErrorReporter,
    skip: Set[String] = Set.empty
  ): Assigned =
    val treeOccs: Seq[(String, Option[Xml.Element])] =
      if !localTables then
        linkIds(tree).distinct.filterNot(skip.contains).map(_ -> None)
      else
        collectOccurrences(tree).filter((id, _) => !skip.contains(id))
    val noteOccs: Seq[(String, String)] =
      footnotes.toSeq.flatMap: (parentId, footnote) =>
        if skip.contains(parentId) then Seq.empty
        else linkIds(footnote.nodes).distinct.filterNot(skip.contains).map(_ -> parentId)
    combine(treeOccs, noteOccs, report)

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
    treeOccs: Seq[(String, Option[Xml.Element])],
    noteOccs: Seq[(String, String)],
    report: PageErrorReporter
  ): Assigned =
    val treeById: Map[String, Seq[Option[Xml.Element]]] =
      treeOccs.foldLeft(Map.empty[String, Seq[Option[Xml.Element]]]):
        case (acc, (id, loc)) => acc.updated(id, acc.getOrElse(id, Seq.empty) :+ loc)
    val notesById: Map[String, Seq[String]] =
      noteOccs.foldLeft(Map.empty[String, Seq[String]]):
        case (acc, (id, parent)) => acc.updated(id, acc.getOrElse(id, Seq.empty) :+ parent)
    val ids: Seq[String] = (treeOccs.map(_._1) ++ noteOccs.map(_._1)).distinct
    val initial: Map[String, Kind] = ids.map: id =>
      val locs: Seq[Option[Xml.Element]] = treeById.getOrElse(id, Seq.empty)
      val parents: Seq[String] = notesById.getOrElse(id, Seq.empty).distinct
      val tables: Seq[Xml.Element] = uniqueEq(locs.flatten)
      val hasDocument: Boolean = locs.exists(_.isEmpty)
      val kind: Kind =
        if hasDocument && tables.nonEmpty then
          report.error(PageError.FootnoteScopeConflict, s"footnote '$id' has conflicting scopes")
          Kind.Document
        else if hasDocument then Kind.Document
        else if tables.size == 1 then Kind.Table
        else if tables.size > 1 then
          report.error(PageError.FootnoteScopeConflict, s"footnote '$id' has conflicting scopes")
          Kind.Document
        else if parents.size == 1 then Kind.Nested(parents.head)
        else if parents.size > 1 then
          report.error(PageError.FootnoteScopeConflict, s"footnote '$id' has conflicting scopes")
          Kind.Document
        else Kind.Document
      id -> kind
    .toMap
    val (flattened: Map[String, Kind], cycleIds: Set[String]) = flattenCycles(initial, report)
    Assigned(promoteDeep(flattened, report), cycleIds)

  private def flattenCycles(
    kinds: Map[String, Kind],
    report: PageErrorReporter
  ): (Map[String, Kind], Set[String]) =
    val nested: Map[String, String] = kinds.collect:
      case (id, Kind.Nested(parentId)) => id -> parentId
    val cycleIds: Set[String] = cycleIdsOf(nested)
    if cycleIds.isEmpty then (kinds, Set.empty)
    else
      cycleIds.foreach: id =>
        report.error(PageError.FootnoteCycle, s"footnote '$id' is in a nested-footnote cycle")
      val flattened: Map[String, Kind] = kinds.map: (id, kind) =>
        if cycleIds.contains(id) then id -> Kind.Document else id -> kind
      (flattened, cycleIds)

  private def cycleIdsOf(nested: Map[String, String]): Set[String] =
    def walk(
      id: String,
      stack: List[String],
      open: Set[String],
      done: Set[String],
      found: Set[String]
    ): (Set[String], Set[String], Set[String]) =
      if done.contains(id) then (open, done, found)
      else if open.contains(id) then
        val loop: Set[String] = (id :: stack.takeWhile(_ != id)).toSet
        (open, done, found ++ loop)
      else
        nested.get(id) match
          case None => (open, done + id, found)
          case Some(parent) =>
            val (open2, done2, found2) = walk(parent, id :: stack, open + id, done, found)
            (open2 - id, done2 + id, found2)
    nested.keys.foldLeft((Set.empty[String], Set.empty[String], Set.empty[String])):
      case ((open, done, found), id) =>
        val (_, done2, found2) = walk(id, Nil, open, done, found)
        (Set.empty, done2, found2)
    ._3

  private def promoteDeep(kinds: Map[String, Kind], report: PageErrorReporter): Map[String, Kind] =
    def depthAndOutermost(id: String): (Int, String) =
      def walk(current: String, depth: Int, seen: Set[String]): (Int, String) =
        if seen.contains(current) then (depth, current)
        else kinds.get(current) match
          case Some(Kind.Nested(parent)) => walk(parent, depth + 1, seen + current)
          case _ => (depth, current)
      walk(id, 0, Set.empty)
    kinds.map: (id, kind) =>
      kind match
        case Kind.Nested(_) =>
          val (depth, outermost) = depthAndOutermost(id)
          if depth > 1 then
            report.error(PageError.FootnoteNesting, s"footnote '$id' is nested deeper than one level")
            id -> Kind.Nested(outermost)
          else id -> kind
        case _ => id -> kind

  private def remapEmitted(
    tree: Xml.Element,
    combined: Map[String, Footnote],
    kinds: Map[String, Kind],
    cycleIds: Set[String],
    hostTreeIds: Set[String],
    emitLeftovers: Boolean
  ): Map[String, Footnote] =
    val treeDocumentIds: Seq[String] =
      linkIds(tree).distinct.filter(id => kinds.get(id).contains(Kind.Document))
    val mentionedInBodies: Seq[String] =
      combined.keys.toSeq.sorted.flatMap: parentId =>
        combined.get(parentId).toSeq.flatMap(footnote => linkIds(footnote.nodes))
      .distinct
    val fromBodies: Seq[String] = mentionedInBodies.filter(cycleIds.contains)
    // Chunks omit leftover bodies; numbering those ids would href a missing target.
    val leftoverCycle: Seq[String] =
      if !emitLeftovers then Seq.empty
      else
        cycleIds.toSeq.sorted.filterNot(id => treeDocumentIds.contains(id) || fromBodies.contains(id))
    val taken: Set[String] = (treeDocumentIds ++ fromBodies ++ leftoverCycle).toSet
    // Inner-only conflict has no host tree site; a call site on another chunk is not leftover.
    val leftoverDocument: Seq[String] =
      if !emitLeftovers then Seq.empty
      else
        mentionedInBodies.filter: id =>
          !taken.contains(id) &&
            kinds.get(id).contains(Kind.Document) &&
            !hostTreeIds.contains(id)
    val documentIds: Seq[String] =
      (treeDocumentIds ++ fromBodies ++ leftoverCycle ++ leftoverDocument).distinct
    val occurrences: Seq[(String, Option[Xml.Element])] = collectOccurrences(tree)
    val tableOccs: Seq[(String, Xml.Element)] = occurrences.flatMap: (id, loc) =>
      if kinds.get(id).contains(Kind.Table) then loc.map(id -> _) else None
    val tables: Seq[Xml.Element] = uniqueEq(tableOccs.map(_._2))
    val documentEmitted: Seq[(String, Footnote)] =
      documentIds.zipWithIndex.flatMap: (id, index) =>
        combined.get(id).map: footnote =>
          id -> remapped(footnote, id, index + 1, FootnoteScope.Document, footnote.nodes)
    val tableEmitted: Seq[(String, Footnote)] =
      tables.zipWithIndex.flatMap: (table, index) =>
        val k: Int = index + 1
        val ids: Seq[String] = tableOccs.filter((_, t) => t eq table).map(_._1).distinct
        ids.zipWithIndex.flatMap: (id, n) =>
          combined.get(id).map: footnote =>
            id -> remapped(footnote, id, n + 1, FootnoteScope.Table(k), footnote.nodes)
    val parents: Map[String, Footnote] = (documentEmitted ++ tableEmitted).toMap
    val nestedEmitted: Seq[(String, Footnote)] =
      parents.toSeq.flatMap: (parentId, parent) =>
        nestedIdsFor(parentId, kinds, combined).zipWithIndex.flatMap: (id, n) =>
          combined.get(id).map: footnote =>
            id -> remapped(
              footnote,
              id,
              n + 1,
              FootnoteScope.Nested(parentId),
              footnote.nodes,
              Some(parent.bodyId)
            )
    (documentEmitted ++ tableEmitted ++ nestedEmitted).toMap

  private def nestedIdsFor(
    parentId: String,
    kinds: Map[String, Kind],
    combined: Map[String, Footnote]
  ): Seq[String] =
    val nested: Set[String] = kinds.collect:
      case (id, Kind.Nested(p)) if p == parentId => id
    .toSet
    val fromNodes: Seq[String] = combined.get(parentId).toSeq
      .flatMap(footnote => linkIds(footnote.nodes).distinct)
      .filter(nested.contains)
    val leftover: Seq[String] = nested.toSeq.sorted.filterNot(fromNodes.contains)
    fromNodes ++ leftover

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
            .setChildren(notes.map(emitBody(_, footnotes)))
          Xml
            .element(XmlElement.Div)
            .addClass("table-with-notes")
            .setChildren(Seq(withChildren: Xml.Node, list: Xml.Node))

  private def emitBody(footnote: Footnote, footnotes: Map[String, Footnote]): Xml.Element =
    footnote.body(nestedChildren(footnote, footnotes))

  private def nestedChildren(parent: Footnote, footnotes: Map[String, Footnote]): Seq[Footnote] =
    footnotes.values
      .filter: child =>
        child.scope match
          case FootnoteScope.Nested(parentId) => parentId == parent.correlationId
          case _ => false
      .toSeq
      .sortBy(_.number)

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

  private def isDocumentScope(footnote: Footnote): Boolean =
    footnote.scope == FootnoteScope.Document

  private def stripInnerBodies(nodes: Xml.Nodes): Xml.Nodes =
    Xml.flatMapNodes(nodes): node =>
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
  val scope: FootnoteScope = FootnoteScope.Document,
  parentBodyId: Option[String] = None
):
  def label: String = scope match
    case FootnoteScope.Document => number.toString
    case FootnoteScope.Table(_) => Footnote.letterLabel(number - 1)
    case FootnoteScope.Nested(_) => Footnote.letterLabel(number - 1)

  private def linkId: String = scope match
    case FootnoteScope.Document => s"_footnote_src_$number"
    case FootnoteScope.Table(k) => s"_table_${k}_fn_src_$label"
    case FootnoteScope.Nested(_) => s"${parentBodyId.getOrElse("")}_n_src_$label"

  private[markup] def bodyId: String = scope match
    case FootnoteScope.Document => s"_footnote_$number"
    case FootnoteScope.Table(k) => s"_table_${k}_fn_$label"
    case FootnoteScope.Nested(_) => s"${parentBodyId.getOrElse("")}_n_$label"

  private def scopeName: Option[String] = scope match
    case FootnoteScope.Document => None
    case FootnoteScope.Table(_) => Some("table")
    case FootnoteScope.Nested(_) => Some("nested")

  def link(withId: Boolean = true): Xml.Element =
    val result: Xml.Element = Xml
      .element(XmlElement.A)
      .add(Footnote.LinkClass)
      .setHref(s"#$bodyId")
      .setText(label)
    val withScope: Xml.Element = scopeName.fold(result)(result.set(Footnote.ScopeAttr, _))
    if withId then withScope.setId(linkId) else withScope

  def body(nested: Seq[Footnote] = Seq.empty): Xml.Element =
    val kids: Xml.Nodes =
      if nested.isEmpty then backLink +: nodes
      else
        val list: Xml.Element = Xml
          .element(XmlElement.Span)
          .addClass("footnotes")
          .addClass("nested-footnotes")
          .setChildren(nested.map(child => child.body()))
        (backLink +: nodes) :+ list
    val result: Xml.Element = Xml
      .element(XmlElement.Span)
      .add(Footnote.BodyClass)
      .setId(bodyId)
      .setChildren(kids)
    scopeName.fold(result)(result.set(Footnote.ScopeAttr, _))

  private def backLink: Xml.Element = Xml
    .element(XmlElement.A)
    .add(Footnote.BackLinkClass)
    .setHref(s"#$linkId")
    .setText(label)
