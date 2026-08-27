package org.podval.tools.publish.page

import org.podval.tools.publish.site.{Path, Site}
import org.podval.tools.publish.util.Icon
import org.podval.xml.Html
import zio.blocks.html.*

object DirectoryPage:
  val fileName: String = "index"

final class DirectoryPage(site: Site, path: Path) extends FullMarkupPage(site, path.html):
  override def isDirectory: Boolean = true

  override def hasSyntheticContent: Boolean = true

  override protected def syntheticContentOpt: Option[Html.Element] =
    if content.flatMap(_.entityListsIndex).isDefined then None
    else Some(syntheticContent)

  private def syntheticContent: Html.Element =
    div(className := "directory", listing(children))

  private def listing(pages: Seq[Page]): Html.Element =
    ul(className := "page-list", pages.map(page => li(page.listRef())))

  override protected def iconDefault: Icon = if isPost then Icon.calendar else Icon.folder

  override def titleDefault: String = postDate match
    case Some(postDate) => postDate.toString // daily note
    case None => titleFromPath

  override def titleFromPath: String =
    if path.path.length > 1
    then path.path.init.last
    else path.fileName // "index"

  def prev(page: Page): Option[Page] = listFor(page).takeWhile(_ != page).reverse.headOption
  def next(page: Page): Option[Page] = listFor(page).dropWhile(_ != page).dropWhile(_ == page).headOption

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

  private lazy val directories: List[Page] = site
    .pages
    .pages
    .filter(_.isDirectory)
    .filter(_.path.path.length > 1)
    .filter(_.path.path.init.init == path.path.init) // TODO unify with the Page.parent path calculations
    .sortBy(_.title)

  private lazy val pages: List[Page] = site
    .pages
    .pages
    .filterNot(_.isDirectory)
    .filterNot(_.isInstanceOf[PdfPage]) // PDF is an alternate of the HTML page, not a sibling
    .filter(_.path.path.init == path.path.init)  // TODO unify with the Page.parent path calculations
    .sortBy(_.title.toLowerCase)
