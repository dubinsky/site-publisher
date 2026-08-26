package org.podval.tools.publish.site

import org.podval.tools.publish.markup.{LinkKind, Markup, XmlMarkup}
import org.podval.tools.publish.page.{AssetWithSourcePath, DirectoryPage, EmbeddedAsset, FrontMatter, FullMarkupPage,
  Page, PageSource, PdfPage, SimpleMarkupPage}
import org.podval.tools.publish.util.Files
import org.podval.xml.Xml
import java.io.File

// TODO mark markup as asset with `asset: true` in the stand-alone front matter.
final class Pages(site: Site):
  import Pages.{ForMarkup, ForName}

  // TODO from Grok:
  //- Description: Page lookup is linear (`pages.find`) and `find(..., kind)` ignores `kind` entirely.
  // Link resolution, directory child listing patterns, duplicate detection, tags, and posts all scan full lists. 
  // For large vaults this is quadratic overall (each page × each link × all pages).
  //- Suggestion: Build indexes after scan: by exact path, by file name / title / titleFromPath, optionally by `LinkKind`.
  // Use them in `get`, `find`, Tags, Posts, DirectoryPage children.
  private var pagesVar: List[Page] = List.empty

  def pages: List[Page] = pagesVar

  // Header pages
  lazy val headerPages: List[HeaderPage] = pages.flatMap(_.headerPage).sortBy(_.priority)

  // TODO make a map for quick lookups:
  private def get(path: Path): Option[Page] = pages.find(_.path == path)

  def load(): Unit =
    // Add embedded assets
    EmbeddedAsset.embeddedAssets(site).foreach(add)

    // Add automatic pages
    val automaticPages: Seq[Page] = Seq(
      site.errors,
      site.tags,
      site.posts
    )
    automaticPages.foreach(add)

    // Scan the directories and add all source pages
    scan(Seq.empty, site.sourceDirectory, None)

    // Post listing batches after scan so `Posts.posts` is complete.
    site.posts.paged.foreach(add)

    // Add synthetic assets that were not supplied explicitly
    if get(Sitemap.path).isEmpty then add(Sitemap(site))
    if get(Robots.path).isEmpty then add(Robots(site))
    if get(Feed.path).isEmpty then add(Feed(site))

    // Report conflicting pages
    pages
      .groupBy(_.path)
      .filter(_._2.length > 1)
      .toList
      .foreach((path: Path, pages: List[Page]) => site.error(
        path,
        PageError.Duplicate,
        s"Duplicates for the path $path: ${pages.map(_.title).tail.mkString(", ")}"
      ))
    
    site.errors.throwIfErrors()

  private def add(page: Page): Unit =
    pagesVar = pagesVar.appended(page)
    // Add implied directories
    page.parent
    // Add alias pages
    page.aliases.foreach(add)
    
    page match
      case page: FullMarkupPage =>
        // Add chunk pages
        if page.chunk then page.chunks.foreach(add)
        if page.pdf then add(PdfPage(page))
      case _ =>  
      
  // Note: only (implied) directories are added without sourcePath
  def getOrAddDirectory(path: Path): DirectoryPage =
    require(path.fileName == DirectoryPage.fileName)
    val (page: DirectoryPage, addIt: Boolean) = get(path.html) match
      case None =>
        (DirectoryPage(site, path.html), true)
      case Some(page) => page match
        case page: DirectoryPage =>
          (page, false)
        case page =>
          throw IllegalArgumentException(s"Not a Directory: $path")

    if addIt then add(page)
    page
  
  private def scan(
    path: Seq[String],
    directory: File,
    externalIndex: Option[ForMarkup]
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

    // TODO separate assets from markup at the beginning
    var forNames: Map[String, ForName] = files
      .map(file =>
        val (name: String, extension: Option[String]) = Files.nameAndExtension(file.getName)
        Path(path :+ name, extension)
      )
      .groupBy(_.fileName)
      .view
      .mapValues(forName)
      .toMap

    def getForMarkup(name: String): Option[ForMarkup] = forNames.get(name).flatMap: forName =>
      val result = forName.markup
      // Remove from the list of files to add
      if result.isDefined then forNames = forNames.updated(name, forName.copy(markup = None))
      result

    val addDirectory: Boolean = !site.posts.isDirectoryEmptiedOut(path)

    val internalIndex: Option[ForMarkup] = getForMarkup(DirectoryPage.fileName)

    // TODO error when internalIndex.isDefined && externalIndex.isDefined
    val index: Option[ForMarkup] = internalIndex.orElse(externalIndex)
    // TODO error if index exists but !addDirectory

    val sourcePath: Path = Path(path :+ DirectoryPage.fileName)

    val directoryPage: Option[Page] = Option.when(addDirectory):
      val path: Path = toPath(sourcePath)
      index match
        case None => getOrAddDirectory(path)
        case Some(index) => addMarkup(index.markup, index.standAloneFrontMatter, path)

    val directoryPageSource: Option[PageSource] = directoryPage.flatMap(_.source)

    // TODO if directoryPage has structure, use that instead of file list!!!

    val directory2index: List[(File, Option[ForMarkup])] = directories
      .map(directory => directory -> getForMarkup(directory.getName))

    forNames.values.foreach: forName =>
      forName.markup.foreach(forMarkup => addMarkup(forMarkup.markup, forMarkup.standAloneFrontMatter, toPath(forMarkup.markup)))
      forName.assets.foreach(sourcePath => add(AssetWithSourcePath(site, sourcePath, toPath(sourcePath))))

    // TODO for Store-described directories, do not scan directory listing
    directory2index.foreach: (directory, externalIndex) =>
      scan(
        path :+ directory.getName,
        directory,
        externalIndex
      )

  private def forName(paths: List[Path]): ForName =
    val (markup: List[Path], nonMarkup: List[Path]) = paths.partition(_.extension.flatMap(Markup.forExtension).isDefined)
    // TODO error if markup.length > 1
    if markup.isEmpty
    then
      ForName(
        markup = None,
        assets = nonMarkup
      )
    else
      val (frontMatter, nonFrontMatter) = nonMarkup.partition(path => FrontMatter.isStandAloneExtension(path.extension))
      // TODO error if frontMatter.length > 1
      ForName(
        markup = Some(ForMarkup(
          markup = markup.head,
          standAloneFrontMatter = frontMatter.headOption
        )),
        assets = nonFrontMatter
      )

  private def addMarkup(
    sourcePath: Path,
    frontMatterStandAlone: Option[Path],
    path: Path
  ): Page =
    val (markup: Markup, parsed: Option[(FrontMatter, Xml.Element)]) =
      if !sourcePath.extension.contains(XmlMarkup.extension)
      then
        // Determine markup by the file extension
        // Note: we can only get here after forName() verified that the extension is a markup one, so - get:
        val markup: Markup = sourcePath.extension.flatMap(Markup.forExtension).get
        (markup, None)
      else
        // Parse and disambiguate XML markup by its XML dialect's root elements
        val (frontMatter, xml: Xml.Element) = XmlMarkup.readAndParse(
          site = site,
          sourcePath = sourcePath,
          frontMatterStandAlone = frontMatterStandAlone,
          message = "Reading to disambiguate XML dialect",
          firstReading = true,
        )
        val markup: Option[Markup] = Markup.forElement(xml.getName)
        // TODO error if unknown XML dialect
        // TODO from Grok:
        //- Description: Unknown XML root element uses `markup.get`, throwing `NoSuchElementException` instead of a `PageError`. A stray or unsupported `.xml` file aborts the whole build with an opaque stack trace rather than a path-scoped diagnostic.
        //- Suggestion: On `None`, report `PageError.FileKind` (or similar) and skip/add a malformed placeholder page, consistent with `MarkupKind.readAndParse` parse failures.
        (markup.get, Some((frontMatter, xml)))

    val (page: Page, addIt: Boolean) = get(path.html) match
      case Some(page) =>
        (page, false)

      case None =>
        val page: Page =
          if path.fileName == DirectoryPage.fileName
          then DirectoryPage(site, path.html)
          else SimpleMarkupPage(site, path.html)
        (page, true)

    page match
      case markupPage: FullMarkupPage =>
        val pageSource: PageSource = PageSource(
          page = markupPage,
          markup = markup,
          sourcePath = sourcePath,
          frontMatterStandAlone = frontMatterStandAlone
        )
        markupPage.setSource(pageSource)
        parsed.foreach((frontMatter, xml) => pageSource.cache(frontMatter, xml))

      case _ => () // TODO error?

    if addIt then add(page)
    page

  // TODO search only pages corresponding to the 'kind'
  def find(
    path: Path,
    isAbsolute: Boolean,
    kind: Option[LinkKind]
  ): Option[Page] = pages.flatMap(page => is(page, path, isAbsolute)).headOption

  private def is(page: Page, path: Path, isAbsolute: Boolean): Option[Page] =
    isPath(page, path, isAbsolute).orElse(
      Option.when(page.sourcePath.exists(isSourcePath(_, path, isAbsolute)))(page)
    )

  private def isPath(page: Page, path: Path, isAbsolute: Boolean): Option[Page] =
    def loop(current: Page, names: Seq[String]): Option[Page] =
      val name: String = names.last
      val init: Seq[String] = names.init
      val done: Boolean = init.isEmpty
      val is: Boolean = current.title == name || current.titleFromPath == name
      Option.when(is)(page).flatMap: (to: Page) =>
        current.parent match
          case None =>
            Option.when(done)(to)
          case Some(parent) =>
            if done
            then Option.when(!isAbsolute)(to)
            else loop(parent, init)

    if !isExtension(page.path, path) then None else
      val names: Seq[String] = path.path
      if names.lastOption.contains(DirectoryPage.fileName)
      then loop(page, names.init)
      else loop(page, names)

  // TODO this should be the same as isPath()?
  private def isSourcePath(sourcePath: Path, path: Path, isAbsolute: Boolean): Boolean =
    isExtension(sourcePath, path) && (
      if isAbsolute
      then sourcePath.path == path.path
      else sourcePath.path.endsWith(path.path)
      )

  private def isExtension(pagePath: Path, path: Path): Boolean =
    path.extension.fold(true)(pagePath.extension.contains)

object Pages:
  private final class ForMarkup(
    val markup: Path,
    val standAloneFrontMatter: Option[Path]
  )

  private final case class ForName(
    markup: Option[ForMarkup],
    assets: List[Path]
  )
