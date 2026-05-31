package org.podval.tools.publish

import org.podval.tools.publish.util.{Files, Icon}
import org.podval.xml.Html
import zio.blocks.html.*
import java.io.File

final class Directory(site: Site, path: Path) extends MarkupPage.WithSyntheticContent(site, path.html):
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

  def scan(
    site: Site,
    directoryPath: Seq[String],
    directory: File
  ): Unit =
    def toPath(sourcePath: Path): Path = site.posts.path(sourcePath).getOrElse(sourcePath)

    val (files: List[File], directories: List[File]) = Files
      .list(directory)
      .filterNot(site.ignore.isIgnored)
      .partition(_.isFile)

    val filePaths: List[Path] = files.map: file =>
      val (name: String, extension: Option[String]) = Files.nameAndExtension(file.getName)
      Path(directoryPath :+ name, extension)

    val nonIndexFilePaths = if site.posts.isDirectoryEmptiedOut(directoryPath) then filePaths else
      val directoryPagePath: Path = toPath(Path(directoryPath :+ fileName))
      val (indexFilePaths, nonIndexFilePaths) = filePaths.partition(_.path == directoryPagePath.path)
      indexFilePaths.headOption.foreach(sourcePath => site.addPage(Some(sourcePath), directoryPagePath))
      nonIndexFilePaths

    nonIndexFilePaths.foreach(sourcePath => site.addPage(Some(sourcePath), toPath(sourcePath)))

    // TODO pair external index files with their directories
    // TODO for Store-described directories, do not scan directory listing
    directories.foreach: directory =>
      scan(
        site,
        directoryPath :+ directory.getName,
        directory
      )
