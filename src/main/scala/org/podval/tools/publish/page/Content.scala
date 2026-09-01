package org.podval.tools.publish.page

import org.podval.tools.publish.markup.{CollectionPart, DocumentHeader, EntityKind, Footnote, Ids, PageType, StoreIndex,
  TeiMarkup, Toc, WikiBlocks, EntityLists as EntityListSpecs}
import org.podval.tools.publish.site.{PageError, Path}
import org.podval.xml.{Html, Xml, XmlUtil}

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

  def markupBody(
    pageContent: PageContent,
    sectionId: Option[String],
    isTerminal: Boolean
  ): Option[Html.Element] = None

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
    xml: Xml.Element
  ): (Option[Xml.Element], Content) =
    if TeiMarkup.isStoreRoot(xml) then
      val store: StoreContent = StoreContent.parse(xml)
      (store.title, store)
    else xml.localName match
      case "entityLists" =>
        val lists: EntityListsContent = EntityListsContent.parse(xml)
        (lists.title, lists)
      case "TEI" =>
        val header: DocumentHeader = DocumentHeader.harvest(xml).get
        CollectionIndex.checkLang(
          source.sourcePath.fileName,
          header.lang,
          (kind, message) => source.error(kind, message)
        )
        val (processed: Xml.Element, title: Option[Xml.Element]) = source.markup.process(xml, source)
        (title, DocumentContent(header, PageContent.prepareAuthored(source, processed)))
      case name =>
        EntityKind.forElement(name) match
          case Some(kind) =>
            val role: Option[String] = xml.get("role").map(_.trim).filter(_.nonEmpty)
            val displayName: Option[String] = entityName(xml, kind)
            val (processed: Xml.Element, title: Option[Xml.Element]) = source.markup.process(xml, source)
            (title, EntityContent(kind, role, displayName, PageContent.prepareAuthored(source, processed)))
          case None =>
            val (processed: Xml.Element, title: Option[Xml.Element]) = source.markup.process(xml, source)
            (title, MarkupContent(PageContent.prepareAuthored(source, processed)))

  private def entityName(xml: Xml.Element, kind: EntityKind): Option[String] =
    xml.getChildren.flatMap(_.asElement)
      .find(el => el.localName == kind.nameElement || el.getName == kind.nameElement)
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
  ): Option[Html.Element] =
    Some(pageContent.renderAuthored(this, sectionId, isTerminal))

final class DocumentContent(
  val header: DocumentHeader,
  prepared: Content.Prepared
) extends AuthoredContent(prepared):
  override def documentHeader: Option[DocumentHeader] = Some(header)

  override def selectedXml(selected: Xml.Element, page: Page): Xml.Element =
    if !page.parent.exists(_.store.exists(_.isCollection)) then selected
    else if selected.localName != "TEI" then selected
    else selected.setChildren(selected.getChildren.filterNot(node =>
      node.asElement.exists(_.localName == "teiHeader")
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

final class MarkupContent(
  prepared: Content.Prepared
) extends AuthoredContent(prepared)

final class StoreContent(
  val index: StoreIndex
) extends Content:
  def selector: Option[String] = index.selector
  def hrefs: Seq[String] = index.hrefs
  def names: Seq[StoreIndex.Name] = index.names
  def title: Option[Xml.Element] = index.title
  def description: Option[Xml.Element] = index.description
  def body: Option[Xml.Element] = index.body
  def isCollection: Boolean = index.isCollection
  def displayName: Option[String] = index.displayName
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
  override def listTitle: Option[String] = displayName
  override def asStore: Option[StoreContent] = Some(this)
  override def suppressDirectoryListing: Boolean = isCollection

  override def markupBody(
    pageContent: PageContent,
    sectionId: Option[String],
    isTerminal: Boolean
  ): Option[Html.Element] =
    if !isCollection then None
    else Some(XmlUtil.xml2html(CollectionIndex.generate(pageContent.source.page, this)))

  def bind(
    page: MarkupPage,
    findBySource: Path => Option[Page],
    isAuthoredDirectory: Seq[String] => Boolean
  ): (List[Page], Set[Seq[String]]) =
    val sourcePath: Path = page.sourcePath.get
    val indexed: Seq[String] = sourcePath.path
    val children: List[Page] = hrefs.toList.flatMap: href =>
      findBySource(sourcePath.resolveFrom(href)) match
        case None =>
          page.site.error(sourcePath, PageError.Unresolved, s"unresolved store include '$href'")
          None
        case Some(child) =>
          Some(child)
    val hops: Set[Seq[String]] =
      hrefs.flatMap(href =>
        StoreContent.hopDirectories(indexed, sourcePath.resolveFrom(href).path, isAuthoredDirectory)
      ).toSet
    (children, hops)

  def reportUnlisted(page: MarkupPage, allPages: Seq[Page]): Unit =
    val sourcePath: Path = page.sourcePath.get
    val indexed: Seq[String] = sourcePath.path
    val listed: Set[Path] = hrefs.map(sourcePath.resolveFrom).toSet
    allPages.foreach: extra =>
      extra.sourcePath.foreach: extraSource =>
        if StoreContent.isUnlisted(extraSource, sourcePath, indexed, listed) &&
           !CollectionIndex.isTranslation(extra) then
          page.site.error(
            extraSource,
            PageError.NotInStore,
            s"not listed in store $sourcePath"
          )

object StoreContent:
  def parse(xml: Xml.Element): StoreContent =
    new StoreContent(StoreIndex(xml).get)

  private def hopDirectories(
    indexed: Seq[String],
    target: Seq[String],
    isAuthoredDirectory: Seq[String] => Boolean
  ): Set[Seq[String]] =
    if !target.startsWith(indexed) then Set.empty
    else
      val between: Seq[String] = target.drop(indexed.length).dropRight(1)
      between.indices.map(i => indexed ++ between.take(i + 1)).toSet.filterNot(isAuthoredDirectory)

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
  val index: EntityListSpecs.Index
) extends Content:
  override def xml: Xml.Element = Xml.element("entityLists")
  override def suppressDirectoryListing: Boolean = true
  override def asEntityLists: Option[EntityListsContent] = Some(this)

  override def markupBody(
    pageContent: PageContent,
    sectionId: Option[String],
    isTerminal: Boolean
  ): Option[Html.Element] =
    // Hrefs are already published paths / intrapage `#id`; do not mark-and-resolve
    // (`#jews` would lose the fragment because index `xml` has no ids).
    Some(XmlUtil.xml2html(EntityLists.generate(pageContent.source.page, index)))

  def listPages(
    directory: DirectoryPage,
    existing: Path => Option[Page]
  ): List[EntityListPage] =
    index.lists.toList.flatMap: spec =>
      val members: Seq[Page] = EntityLists.members(directory, spec)
      if members.isEmpty then None
      else
        val listPath: Path = EntityLists.listPath(directory, spec)
        existing(listPath) match
          case Some(existingPage) =>
            directory.site.error(
              listPath,
              PageError.Duplicate,
              s"entity list '${spec.id}' collides with $existingPage"
            )
            None
          case None =>
            Some(EntityListPage(directory.site, listPath, spec, members))

object EntityListsContent:
  def parse(xml: Xml.Element): EntityListsContent =
    new EntityListsContent(
      title = xml.getChildren.flatMap(_.asElement)
        .find(el => el.localName == "title" || el.localName == "tei-title")
        .filter(_.getText.trim.nonEmpty),
      index = EntityListSpecs.harvest(xml).get
    )
