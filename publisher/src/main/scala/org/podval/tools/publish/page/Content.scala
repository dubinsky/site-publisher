package org.podval.tools.publish.page

import org.podval.metadata.{Language, Name}
import org.podval.tools.publish.markup.{CollectionPart, DocumentHeader, EntityKind, Footnote, Ids, PageType, StoreIndex,
  TeiMarkup, Toc, WikiBlocks, EntityListSpec}
import org.podval.tools.publish.site.{PageError, Path}
import org.podval.xml.{Xml, XmlAttribute, XmlElement}

/** Kind of a source page. At most one of store / entity-lists / TEI document / entity / markup. */
sealed abstract class Content:
  def xml: Xml.Element
  def toc: Toc = Toc.empty
  def ids: Ids = Ids.empty
  def blocks: WikiBlocks = WikiBlocks.empty
  def footnotes: Map[String, Footnote] = Map.empty
  def glossaryDefinitions: Map[String, Xml.Nodes] = Map.empty
  def bibliographyDefinitions: Map[String, Xml.Nodes] = Map.empty

  def wide: Boolean = false
  def listTitle: Option[String] = None
  def suppressDirectoryListing: Boolean = false
  def documentHeader: Option[DocumentHeader] = None
  def entityKind: Option[EntityKind] = None
  def entityRole: Option[String] = None
  def entityDisplayName: Option[String] = None

  def asStore: Option[StoreContent] = None
  def asEntityLists: Option[EntityListsContent] = None
  def asDocument: Option[DocumentContent] = None
  def asEntity: Option[EntityContent] = None
  def asMarkup: Option[MarkupContent] = None
  def publishesTei: Boolean = false

  def markupBody(
    pageContent: PageContent,
    sectionId: Option[String],
    isTerminal: Boolean
  ): Option[Xml.Element] = None

object Content:
  final class Prepared(
    val xml: Xml.Element,
    val toc: Toc,
    val ids: Ids,
    val blocks: WikiBlocks,
    val footnotes: Map[String, Footnote],
    val glossaryDefinitions: Map[String, Xml.Nodes],
    val bibliographyDefinitions: Map[String, Xml.Nodes]
  )

  def parse(
    source: PageSource,
    xml: Xml.Element,
    firstReading: Boolean
  ): (Option[Xml.Element], Content) =
    if TeiMarkup.isStoreRoot(xml) then
      val merged: StoreIndex = StoreIndex(xml).get
      val accepted: Boolean = checkLabelRef(source, merged.ref, merged.plural, merged.names.nonEmpty)
      val index: StoreIndex =
        if accepted then merged else merged.copy(ref = None, plural = false)
      val store: StoreContent = new StoreContent(index)
      if firstReading then
        (store.title.toSeq ++ store.description.toSeq ++ store.body.toSeq).foreach: fragment =>
          TeiMarkup.reportSourceErrors(fragment, source, namesWithoutRef = true)
      (store.title, store)
    else xml.getName.localName match
      case "entityLists" =>
        val harvested: EntityListSpec.Index = EntityListSpec.harvest(xml).get
        val accepted: Boolean =
          checkLabelRef(source, harvested.ref, harvested.plural, harvested.title.exists(_.trim.nonEmpty))
        val index: EntityListSpec.Index =
          if accepted then harvested else harvested.copy(ref = None, plural = false)
        val fromRef: Option[String] = index.ref.map: id =>
          given Language.Spec = source.languageSpec
          source.page.site.terms.pluralHeading(id)
        val text: Option[String] = fromRef.filter(_.nonEmpty).orElse(index.title.filter(_.nonEmpty))
        val lists: EntityListsContent = new EntityListsContent(
          title = text.map(value => Xml.element(XmlElement.Title).setText(value)),
          index = index
        )
        (lists.title, lists)
      case "TEI" =>
        val header: DocumentHeader = DocumentHeader.harvest(xml).get
        if firstReading then
          TeiMarkup.reportSourceErrors(xml, source, namesWithoutRef = true)
        val (processed: Xml.Element, title: Option[Xml.Element]) = source.markup.process(xml, source)
        (title, DocumentContent(header, PageContent.prepareAuthored(source, processed)))
      case name =>
        EntityKind.forElement(name) match
          case Some(kind) =>
            val role: Option[String] = xml.get(XmlAttribute.Role).map(_.trim).filter(_.nonEmpty)
            val displayName: Option[String] = entityName(xml, kind)
            if firstReading then
              TeiMarkup.reportSourceErrors(xml, source, namesWithoutRef = false)
              displayName.foreach: display =>
                val expected: String = TeiMarkup.expectedEntityFileName(display)
                val id: String = source.sourcePath.fileName
                // `@stable="true"` keeps a published id when the heading differs.
                if expected != id && !isStableEntity(xml) then
                  source.error(PageError.MisnamedEntity, s"should be named '$expected'")
            val (processed: Xml.Element, title: Option[Xml.Element]) = source.markup.process(xml, source)
            (title, EntityContent(kind, role, displayName, PageContent.prepareAuthored(source, processed)))
          case None =>
            val (processed: Xml.Element, title: Option[Xml.Element]) = source.markup.process(xml, source)
            (title, MarkupContent(PageContent.prepareAuthored(source, processed)))

  /** `ref` and `plural` are a pair. Decode has no source path, so the page error carries it. */
  private def checkLabelRef(
    source: PageSource,
    ref: Option[String],
    plural: Boolean,
    authored: Boolean
  ): Boolean =
    val message: Option[String] =
      if plural && ref.isEmpty then Some("plural without ref")
      else ref match
        case None => None
        case Some(_) if !plural => Some("ref without plural")
        case Some(_) if authored => Some("ref with names")
        case Some(id) =>
          val known: Boolean =
            try
              source.page.site.terms.getById(id)
              true
            catch case _: IllegalArgumentException => false
          if known then None else Some(s"Unknown term: $id")
    message match
      case None => true
      case Some(text) =>
        // A year/tree/flat re-home parses this file again. Keep the scan's row.
        if !source.page.site.errors.hasBadLabel(source.sourcePath) then
          source.error(PageError.BadLabel, text)
        false

  private def isStableEntity(xml: Xml.Element): Boolean =
    xml.get("stable").exists(_.trim == "true")

  private def entityName(xml: Xml.Element, kind: EntityKind): Option[String] =
    xml.childElements
      .find(_.isNamed(kind.nameElement))
      .map(_.getText.trim)
      .filter(_.nonEmpty)

sealed abstract class AuthoredContent(
  prepared: Content.Prepared
) extends Content:
  final override def xml: Xml.Element = prepared.xml
  final override def toc: Toc = prepared.toc
  final override def ids: Ids = prepared.ids
  final override def blocks: WikiBlocks = prepared.blocks
  final override def footnotes: Map[String, Footnote] = prepared.footnotes
  final override def glossaryDefinitions: Map[String, Xml.Nodes] = prepared.glossaryDefinitions
  final override def bibliographyDefinitions: Map[String, Xml.Nodes] = prepared.bibliographyDefinitions

  def selectedXml(selected: Xml.Element, page: Page): Xml.Element = selected

  final override def markupBody(
    pageContent: PageContent,
    sectionId: Option[String],
    isTerminal: Boolean
  ): Option[Xml.Element] =
    Some(pageContent.renderAuthored(this, sectionId, isTerminal))

final class DocumentContent(
  val header: DocumentHeader,
  prepared: Content.Prepared
) extends AuthoredContent(prepared):
  override def documentHeader: Option[DocumentHeader] = Some(header)
  override def asDocument: Option[DocumentContent] = Some(this)
  override def publishesTei: Boolean = documentHeader.isDefined

  override def selectedXml(selected: Xml.Element, page: Page): Xml.Element =
    if !page.parent.exists(_.store.exists(_.isCollection)) then selected
    else if !selected.isNamed("TEI") then selected
    else selected.setChildren(selected.getChildren.filterNot(node =>
      node.asElement.exists(_.isNamed("teiHeader"))
    ))

final class EntityContent(
  kind: EntityKind,
  role: Option[String],
  displayName: Option[String],
  prepared: Content.Prepared
) extends AuthoredContent(prepared):
  override def entityKind: Option[EntityKind] = Some(kind)
  override def entityRole: Option[String] = role
  override def entityDisplayName: Option[String] = displayName
  override def asEntity: Option[EntityContent] = Some(this)

final class MarkupContent(
  prepared: Content.Prepared
) extends AuthoredContent(prepared):
  override def asMarkup: Option[MarkupContent] = Some(this)

final class StoreContent(
  val index: StoreIndex
) extends Content:
  def ref: Option[String] = index.ref
  def selector: Option[String] = index.selector
  def hrefs: Seq[String] = index.hrefs
  def names: Seq[Name] = index.names
  def title: Option[Xml.Element] = index.title
  def description: Option[Xml.Element] = index.description
  def body: Option[Xml.Element] = index.body
  def isCollection: Boolean = index.isCollection
  /** Harvested TEI `@alias`; wrap emits `org.podval.store.Alias`. */
  def alias: Option[String] = index.alias
  def parts: Seq[CollectionPart] = index.parts
  def pageType: PageType = index.pageType
  def pageTypeName: Option[String] = index.pageTypeName

  private var boundChildrenVar: List[Page] = Nil
  def setBoundChildren(children: List[Page]): Unit = boundChildrenVar = children
  def boundChildren: List[Page] = boundChildrenVar

  override def xml: Xml.Element =
    Xml.element(if isCollection then "collection" else "store")

  override def wide: Boolean = isCollection
  override def asStore: Option[StoreContent] = Some(this)
  override def suppressDirectoryListing: Boolean = isCollection

  override def markupBody(
    pageContent: PageContent,
    sectionId: Option[String],
    isTerminal: Boolean
  ): Option[Xml.Element] =
    if !isCollection then None
    else Some(CollectionIndex.generate(pageContent.source.page, this))

  def bind(
    page: MarkupPage,
    findBySource: Path => Option[Page]
  ): List[Page] =
    val sourcePath: Path = page.sourcePath.get
    hrefs.toList.flatMap: href =>
      findBySource(sourcePath.resolveFrom(href)) match
        case None =>
          page.site.error(sourcePath, PageError.Unresolved, s"unresolved store include '$href'")
          None
        case Some(child) =>
          Some(child)

  def reportUnlisted(page: MarkupPage, allPages: Seq[Page]): Unit =
    val sourcePath: Path = page.sourcePath.get
    val indexed: Seq[String] = sourcePath.path
    val listed: Set[Path] = hrefs.map(sourcePath.resolveFrom).toSet
    allPages.foreach: extra =>
      extra.sourcePath.foreach: extraSource =>
        if StoreContent.isUnlisted(extraSource, sourcePath, indexed, listed) &&
           !CollectionIndex.isTranslation(extra) &&
           !CollectionIndex.isReading(extra) then
          page.site.error(
            extraSource,
            PageError.NotInStore,
            s"not listed in store $sourcePath"
          )

object StoreContent:
  def parse(xml: Xml.Element): StoreContent =
    new StoreContent(StoreIndex(xml).get)

  def selectorHops(
    indexed: Seq[String],
    selector: Option[String],
    childSources: Seq[Seq[String]],
    isAuthoredDirectory: Seq[String] => Boolean
  ): Set[Seq[String]] =
    selector.toSeq.flatMap: sel =>
      val hop: Seq[String] = indexed :+ sel
      Option.when(childSources.exists(_.startsWith(hop)) && !isAuthoredDirectory(hop))(hop)
    .toSet

  private def isUnlisted(
    extraSource: Path,
    storeSource: Path,
    indexed: Seq[String],
    listed: Set[Path]
  ): Boolean =
    extraSource != storeSource &&
    extraSource.path.startsWith(indexed) &&
    !listed.contains(extraSource) &&
    !listed.exists(listedSource =>
      extraSource.path.startsWith(listedSource.path) && extraSource.path.length > listedSource.path.length
    )

final class EntityListsContent(
  val title: Option[Xml.Element],
  val index: EntityListSpec.Index
) extends Content:
  override def xml: Xml.Element = Xml.element("entityLists")
  override def suppressDirectoryListing: Boolean = true
  override def asEntityLists: Option[EntityListsContent] = Some(this)

  override def markupBody(
    pageContent: PageContent,
    sectionId: Option[String],
    isTerminal: Boolean
  ): Option[Xml.Element] =
    val generated: Xml.Element = EntityLists.generate(pageContent.source.page, index)
    pageContent.source.page.inlineListProse match
      case None => Some(generated)
      case Some(detached) => Some(proseThenList(detached, generated))

  /** List hrefs are already published paths. The file is resolved here, at write, from the catalog page. */
  private def proseThenList(detached: PageSource, generated: Xml.Element): Xml.Element =
    val prose: PageContent = detached.content
    val heading: Option[Xml.Element] =
      prose.title.orElse(prose.frontMatter.title.map(text => Xml.element(XmlElement.H1).setText(text)))
    val body: Option[Xml.Element] = prose.markupContent(sectionId = None, isTerminal = true)
    Xml.element(XmlElement.Div).setChildren(heading.toSeq ++ body.toSeq :+ generated)
