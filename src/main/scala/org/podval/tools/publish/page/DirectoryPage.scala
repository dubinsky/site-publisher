package org.podval.tools.publish.page

import org.podval.tools.publish.util.Icon
import org.podval.tools.publish.{Path, Site}
import org.podval.xml.Html
import zio.blocks.html.*

object DirectoryPage:
  val fileName: String = "index"

final class DirectoryPage(site: Site, path: Path) extends SyntheticMarkupPage(site, path.html):
  override def isDirectory: Boolean = true

  override protected def syntheticContent: Html.Element =
    div(className := "directory", Page.pageList(directories ++ pages))

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
    if page.isInstanceOf[DirectoryPage]
    then directories
    else pages
    
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
    .filter(_.path.path.init == path.path.init)  // TODO unify with the Page.parent path calculations
    .sortBy(_.title.toLowerCase)
