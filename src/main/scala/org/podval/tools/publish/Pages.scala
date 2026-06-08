package org.podval.tools.publish

import org.podval.tools.publish.markup.{Markup, XmlLikeMarkup, XmlMarkup}
import org.podval.tools.publish.page.{AssetWithSourcePath, DirectoryPage, FrontMatter, MarkupPage, Page, PageSource, SimpleMarkupPage}
import org.podval.tools.publish.util.Files
import org.podval.xml.Xml

import java.io.File

final class Pages(site: Site):
  private var pagesVar: List[Page] = List.empty

  def pages: List[Page] = pagesVar

  def add(page: Page): Unit =
    pagesVar = pagesVar.appended(page)
    // Add implied directories
    page.parent
    // Add alias pages
    page.aliases.foreach(add)

  // Note: only (implied) directories are added without sourcePath
  def addOrFindDirectory(path: Path): DirectoryPage =
    require(path.fileName == DirectoryPage.fileName)
    val (page: DirectoryPage, addIt: Boolean) = pages.find(_.path == path.html) match
      case None =>
        (DirectoryPage(site, path.html), true)
      case Some(page) => page match
        case page: DirectoryPage =>
          (page, false)
        case page =>
          throw IllegalArgumentException(s"Not a Directory")

    if addIt then add(page)
    page

  def add(
    sourcePath: Path,
    path: Path
  ): Page =
    val (markup: Option[Markup], parsed: Option[(FrontMatter, Xml.Element)]) =
      sourcePath.extension.fold((None, None)): extension =>
        if extension != XmlLikeMarkup.extension
        then
          // Determine markup by the file extension
          val markup: Option[Markup] = Markup.all.find(_.isExtension(extension))
          (markup, None)
        else
          // Parse and disambiguate XML markup by its XML dialect's root elements
          val (frontMatter, xml: Xml.Element) = XmlMarkup.readAndParse(
            site = site,
            sourcePath = sourcePath,
            message = "Reading to disambiguate XML dialect",
            firstReading = true,
          )
          val rootElementName: String = xml.getName
          val markup: Option[Markup] = Markup.xmlLike.find(_.xmlDialect.root.contains(rootElementName))
          (markup, Some((frontMatter, xml)))

    val (page: Page, addIt: Boolean) = pages.find(_.path == path.html) match
      case Some(page) =>
        (page, false)

      case None =>
        val page: Page =
          if path.fileName == DirectoryPage.fileName
          then DirectoryPage(site, path.html)
          else markup match
            case None => AssetWithSourcePath(site, sourcePath, path)
            case Some(markup) => SimpleMarkupPage(site, path.html)
        (page, true)

    page match
      case markupPage: MarkupPage =>
        markup.foreach: markup =>
          val pageSource: PageSource = PageSource(
            page = markupPage,
            markup = markup,
            sourcePath = sourcePath
          )
          markupPage.setSource(pageSource)
          parsed.foreach((frontMatter, xml) => pageSource.cache(frontMatter, xml))

      case _ => ()

    if addIt then add(page)
    page


  def scan(
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

    val addDirectory: Boolean = !site.posts.isDirectoryEmptiedOut(path)

    val internalIndex: Option[Path] = fileByName(DirectoryPage.fileName)
    // TODO error when internalIndex.isDefined && externalIndex.isDefined
    val index: Option[Path] = internalIndex.orElse(externalIndex)
    // TODO error if index exists but !addDirectory

    val sourcePath: Path = Path(path :+ DirectoryPage.fileName)

    val directoryPage: Option[Page] = Option.when(addDirectory):
      val path: Path = toPath(sourcePath)
      index match
        case None => site.pages.addOrFindDirectory(path)
        case Some(index) => site.pages.add(index, path)

    val directoryPageSource: Option[PageSource] = directoryPage.flatMap(_.source)

    // TODO if directoryPage has structure, use that instead of file list!!!

    val directory2index: List[(File, Option[Path])] = directories
      .map(directory => directory -> fileByName(directory.getName))

    name2file.values.foreach(sourcePath => site.pages.add(sourcePath, toPath(sourcePath)))

    // TODO for Store-described directories, do not scan directory listing
    directory2index.foreach: (directory, externalIndex) =>
      scan(
        path :+ directory.getName,
        directory,
        externalIndex
      )
