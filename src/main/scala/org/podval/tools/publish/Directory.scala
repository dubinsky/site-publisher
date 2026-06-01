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
    .filter(_.path.path.init.init == path.path.init) // TODO unify with the Page.parent path calculations
    .sortBy(_.title)

  private lazy val pages: List[Page] = site
    .pages
    .filterNot(_.isDirectory)
    .filter(_.path.path.init == path.path.init)  // TODO unify with the Page.parent path calculations
    .sortBy(_.title.toLowerCase)

object Directory:
  val fileName: String = "index"

  def scan(
    site: Site,
    path: Seq[String],
    directory: File,
    externalIndex: Option[Path]
  ): Unit =
    val pathString: String = if path.isEmpty then "/" else path.mkString("/", "/", "/")

    def toPath(sourcePath: Path): Path = site.posts.path(sourcePath).getOrElse(sourcePath)

    val (files: List[File], directories: List[File]) = Files
      .list(directory)
      .filterNot(file =>
        val isIgnored: Boolean = site.ignore.isIgnored(s"$pathString${file.getName}", file.isDirectory)
        if isIgnored then site.log.debug(s"Ignored: $file")
        isIgnored
      )
      .partition(_.isFile)

    var name2file: Map[String, Path] = files
      .map(file =>
        val (name: String, extension: Option[String]) = Files.nameAndExtension(file.getName)
        name -> Path(path :+ name, extension)
      )
      .toMap

    def fileByName(name: String): Option[Path] =
      val result = name2file.get(name)
      if result.isDefined then name2file = name2file.removed(name)
      result

    val sourcePath: Path = Path(path :+ fileName)
    val internalIndex: Option[Path] = fileByName(fileName)

    // TODO error when internalIndex.isDefined && externalIndex.isDefined

    internalIndex.orElse(externalIndex).foreach: index =>
      if !site.posts.isDirectoryEmptiedOut(path)
      then site.addPage(Some(index), toPath(sourcePath))
      else () // TODO error about the indexes

    val directory2index: List[(File, Option[Path])] = directories
      .map(directory => directory -> fileByName(directory.getName))

    name2file.values.foreach(sourcePath => site.addPage(Some(sourcePath), toPath(sourcePath)))

    // TODO for Store-described directories, do not scan directory listing
    directory2index.foreach: (directory, externalIndex) =>
      scan(
        site,
        path :+ directory.getName,
        directory,
        externalIndex
      )
