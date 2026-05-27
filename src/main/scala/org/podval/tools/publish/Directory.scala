package org.podval.tools.publish

import org.podval.tools.publish.util.Icon
import org.podval.xml.Html
import zio.blocks.html.*

final class Directory(site: Site, path: Path) extends MarkupPage.WithSyntheticContent(site, path):
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
    if page.isInstanceOf[Directory]
    then directories
    else pages
    
  private lazy val directories: List[Page] = site
    .pages
    .filter(_.isDirectory)
    .filter(_.path.path.length > 1)
    .filter(_.path.path.init.init == path.path.init)
    .sortBy(_.title)

  private lazy val pages: List[Page] = site
    .pages
    .filterNot(_.isDirectory)
    .filter(_.path.path.init == path.path.init)
    .sortBy(_.title.toLowerCase)

object Directory:
  val fileName: String = "index"
  
  // Implicitly force insertion of the missing `index` pages.
  def addParentDirectories(site: Site): Unit =
    site.pages.foreach(_.parent)

  def parent(site: Site, page: Page): Option[Directory] =
    val parentDirectory: Option[Seq[String]] =
      if page.isDirectory && page.path.path.length > 1 then Some(page.path.path.init.init)
      else if !page.isDirectory && page.path.path.nonEmpty then Some(page.path.path.init)
      else None

    parentDirectory.map: parentDirectory =>
      val parentPath: Path = Path(parentDirectory.appended(Directory.fileName) *).html
      site.find(parentPath)
        .map {
          case page: Directory => page
          case _ => throw IllegalArgumentException(s"Not a Directory")
        }
        .getOrElse:
          val parent: Directory = site.addPage(Directory(site, parentPath))
          // Force insertion of the parent's parent for the newly-inserted parent
          parent.parent
          parent
