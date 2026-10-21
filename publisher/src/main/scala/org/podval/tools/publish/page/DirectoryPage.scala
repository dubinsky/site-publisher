package org.podval.tools.publish.page

import org.podval.tools.publish.site.{Path, SearchPage, Site}
import org.podval.tools.publish.util.{Files, Icon}
import org.podval.xml.Xml
import org.podval.xml.dsl.{*, given}

object DirectoryPage:
  val fileName: String = "index"

final class DirectoryPage(site: Site, path: Path) extends FullMarkupPage(site, path.html):
  override def isDirectory: Boolean = true

  private var passThroughSourceVar: Option[Path] = None

  def setPassThrough(source: Path): Unit = passThroughSourceVar = Some(source)

  override def sourcePath: Option[Path] = passThroughSourceVar.orElse(super.sourcePath)

  override def write(): Unit = passThroughSourceVar match
    case Some(source) => Files.copy(fromFile = site.sourceFile(source), toFile = targetFile)
    case None => super.write()

  override def hasSyntheticContent: Boolean = passThroughSourceVar.isEmpty

  override protected def syntheticContentOpt: Option[Xml.Element] =
    if passThroughSourceVar.isDefined || doc.exists(_.suppressDirectoryListing) then None
    else Some(syntheticContent)

  private def syntheticContent: Xml.Element =
    div(className := "directory", listing(children))

  private def listing(pages: Seq[Page]): Xml.Element =
    ul(className := "page-list", pages.map(page => li(page.listRef())))

  override protected def iconDefault: Icon = if isPost then Icon.calendar else Icon.folder

  override def titleDefault: String = postDate match
    case Some(postDate) => postDate.toString // daily note
    case None => titleFromPath

  override def titleFromPath: String =
    if path.path.length > 1
    then path.path.init.last
    else path.fileName // "index"

  def prev(page: Page): Option[Page] =
    position(page).filter(_ > 0).map(i => navList(page)(i - 1))

  def next(page: Page): Option[Page] =
    position(page).flatMap(i => navList(page).drop(i + 1).headOption)

  private def position(page: Page): Option[Int] =
    val list: List[Page] = navList(page)
    if CollectionIndex.isReading(page) then readingPosition(list, page)
    else
      val exact: Int = list.indexOf(page)
      if exact >= 0 then Some(exact)
      else collectionSlot(list, CollectionIndex.baseName(page))

  /** A reading sits in the original's slot, so its neighbors are that page's neighbors. */
  private def readingPosition(list: List[Page], page: Page): Option[Int] =
    val stem: String = CollectionIndex.withoutReading(CollectionIndex.fileName(page))
    val exact: Int = list.indexWhere(child => CollectionIndex.fileName(child) == stem)
    if exact >= 0 then Some(exact)
    else collectionSlot(list, CollectionIndex.splitLang(stem)._1)

  private def collectionSlot(list: List[Page], base: String): Option[Int] =
    if !store.exists(_.isCollection) then None
    else
      val i: Int = list.indexWhere(child => CollectionIndex.baseName(child) == base)
      Option.when(i >= 0)(i)

  /** Readings stay out of prev/next in every directory. A listing may still link them. */
  private def navList(page: Page): List[Page] =
    listFor(page).filterNot(CollectionIndex.isReading)

  private def listFor(page: Page): List[Page] =
    storeChildrenVar.getOrElse(
      if page.isInstanceOf[DirectoryPage]
      then directories
      else pages
    )

  private def children: List[Page] = storeChildrenVar.getOrElse(directories ++ pages)

  private var storeChildrenVar: Option[List[Page]] = None

  def setStoreChildren(children: List[Page]): Unit = storeChildrenVar = Some(children)

  def storeChildren: Option[List[Page]] = storeChildrenVar

  // Recomputed: `containingDirectory` changes once selector hops are registered.
  private def filesystemChildren: List[Page] = site
    .pages
    .pages
    .filter(page => page != this && page.containingDirectory == path.path.init)
    .filterNot(_.isInstanceOf[PdfPage])
    .filterNot(_.isInstanceOf[TeiDocumentPage])
    .filterNot(_.isInstanceOf[FacsimilePage])
    .filterNot(_.isInstanceOf[StoreIndexPage])
    .filterNot(_.isInstanceOf[YearIndexPage])
    .filterNot(_.isInstanceOf[SearchPage])
    .filterNot(_.isInstanceOf[MapPage])

  private def directories: List[Page] =
    filesystemChildren.filter(_.isDirectory).sortBy(_.title)

  private def pages: List[Page] =
    filesystemChildren.filterNot(_.isDirectory).sortBy(_.title.toLowerCase)
