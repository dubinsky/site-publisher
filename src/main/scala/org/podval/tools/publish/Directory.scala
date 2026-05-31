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

  def parent(site: Site, page: Page): Option[Directory] =
    val parentDirectory: Option[Seq[String]] =
      if page.isDirectory && page.path.path.length > 1 then Some(page.path.path.init.init)
      else if !page.isDirectory && page.path.path.nonEmpty then Some(page.path.path.init)
      else None

    parentDirectory.map: parentDirectory =>
      val parentPath: Path = Path(parentDirectory.appended(Directory.fileName) *).html
      site
        .find(parentPath)
        .map {
          case page: Directory => page
          case _ => throw IllegalArgumentException(s"Not a Directory")
        }
        .getOrElse:
          val parent: Directory = Directory(site, parentPath)
          site.addPage(parent)
          parent

  private def markup(sourcePath: Path): Option[Markup] = sourcePath.extension.flatMap: extension =>
    Markup.all.find(_.isExtension(extension))

  def scan(
    site: Site,
    directoryPath: Seq[String],
    directory: File
  ): Unit =
    def toPath(sourcePath: Path): Path = site.posts.path(sourcePath).getOrElse(sourcePath)

    def addDirectory(indexSourcePath: Option[Path], path: Path): Unit =
      val page: Directory = Directory(site, path)

      for
        sourcePath <- indexSourcePath
        markup <- markup(sourcePath)
      do
        page.setSource(markup, sourcePath)

      site.addPage(page)

    val (files: List[File], directories: List[File]) = Files
      .list(directory)
      .filterNot(isExcluded(site))
      .partition(_.isFile)

    val filePaths: List[Path] = files.map: file =>
      val (name: String, extension: Option[String]) = Files.nameAndExtension(file.getName)
      Path(directoryPath :+ name, extension)

    val directoryPagePath: Path = toPath(Path(directoryPath :+ fileName))
    val (indexFilePaths, nonIndexFilePaths) = filePaths.partition(_.path == directoryPagePath.path)

    if !site.posts.isDirectoryEmptiedOut(directoryPath) then
      addDirectory(indexFilePaths.headOption, directoryPagePath)

    nonIndexFilePaths.foreach: sourcePath =>
      val path: Path = toPath(sourcePath)
      if path.fileName == Directory.fileName
      then addDirectory(Some(sourcePath), path)
      else markup(sourcePath) match
        case None => site.addPage(Asset.AssetWithSource(site, sourcePath, path))
        // TODO search among synthetics only
        case Some(markup) => site.find(sourcePath.html) match
          case Some(page) =>
            page.setSource(markup, sourcePath)
          case None =>
            val page: MarkupPage = MarkupPage.Simple(site, path.html)
            page.setSource(markup, sourcePath)
            site.addPage(page)

    // TODO pair external index files with their directories
    // TODO for Store-described directories, do not scan directory listing
    directories.foreach: directory =>
      scan(
        site,
        directoryPath :+ directory.getName,
        directory
      )

  private def isExcluded(site: Site)(file: File): Boolean =
    val name: String = file.getName
    if site.include.contains(name) then false
    else if site.exclude.contains(name) then
      site.log.debug(s"excluded: $name")
      true
    else
      special.contains(name) ||
      specialStartsWith.exists(name.startsWith)

  private val special: Set[String] = Set(
    ".jekyll-cache",
    ".sass-cache",
    "Gemfile",
    "Gemfile.lock",
    "LICENSE",
    "README.md",
    "build",
    "build.gradle",
    "bundle",
    "gradle",
    "gradlew",
    "gradlew.bat",
    "node_modules",
    "settings.gradle",
    "src",
    "vendor",
  )

  private val specialStartsWith: Set[String] = Set(
    ".",
    "_",
    "~",
    "#"
  )
