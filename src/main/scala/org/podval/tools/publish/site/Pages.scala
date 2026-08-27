package org.podval.tools.publish.site

import org.podval.tools.publish.markup.{LinkKind, Markup, StoreIndex, TeiMarkup, XmlMarkup}
import org.podval.tools.publish.page.{Alias, AssetWithSourcePath, DirectoryPage, EmbeddedAsset, FrontMatter,
  MarkupPage, Page, PageSource, PdfPage, SimpleMarkupPage}
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
  private var headerPagesVar: List[Page] = List.empty
  def headerPages: List[Page] = headerPagesVar

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

    installHome()
    headerPagesVar = resolveHeaderPages()
    resolveStores()

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

  private def installHome(): Unit =
    site.config.home.map(_.trim).filter(_.nonEmpty).foreach: home =>
      val requested: Path = Path.fromHref(home)
      val target: Option[Page] = pageForSpec(home)
      target match
        case None =>
          site.error(requested, PageError.Unresolved, s"home page not found: $home")
        case Some(target) =>
          val index: Path = Path(DirectoryPage.fileName).html
          get(index) match
            case Some(page) if page.source.isDefined =>
              site.error(
                index,
                PageError.Duplicate,
                s"home: '$home' cannot coexist with an authored index"
              )
            case Some(_: DirectoryPage) =>
              pagesVar = pagesVar.filterNot(page => page.path == index && page.isInstanceOf[DirectoryPage])
              add(new Alias(site, target, index))
            case None =>
              add(new Alias(site, target, index))
            case Some(_) =>
              site.error(
                index,
                PageError.Duplicate,
                s"home: '$home' cannot occupy $index"
              )

  private def resolveHeaderPages(): List[Page] =
    site.config.headerPages.foldLeft(List.empty[Page]): (acc, spec) =>
      val trimmed: String = spec.trim
      if trimmed.isEmpty then acc
      else
        pageForSpec(trimmed) match
          case None =>
            site.error(Path.fromHref(trimmed), PageError.Unresolved, s"header page not found: $trimmed")
            acc
          case Some(page) =>
            if acc.exists(_.path == page.path) then acc
            else acc :+ page

  private def pageForSpec(spec: String): Option[Page] =
    val requested: Path = Path.fromHref(spec.trim)
    find(requested, isAbsolute = true, kind = None)
      .orElse(Option.when(requested.extension.isEmpty)(find(requested.html, isAbsolute = true, kind = None)).flatten)

  private def add(page: Page): Unit =
    pagesVar = pagesVar.appended(page)
    // Add implied directories
    page.parent
    page.asFullMarkupPage.foreach: page =>
      page.aliases.foreach(add)
      if page.chunk then page.chunks.foreach(add)
      if page.pdf then add(PdfPage(page))
      
  private var selectorHopsVar: Set[Seq[String]] = Set.empty

  def isSelectorHop(directory: Seq[String]): Boolean = selectorHopsVar.contains(directory)

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
      case page: MarkupPage =>
        val pageSource: PageSource = PageSource(
          page = page,
          markup = markup,
          sourcePath = sourcePath,
          frontMatterStandAlone = frontMatterStandAlone
        )
        page.setSource(pageSource)
        parsed.foreach((frontMatter, xml) => pageSource.cache(frontMatter, xml))
      case _ => () // TODO error?

    if addIt then add(page)
    page

  private def resolveStores(): Unit =
    var hops: Set[Seq[String]] = Set.empty
    pages.foreach:
      case page: MarkupPage if page.source.exists(_.markup == TeiMarkup) =>
        page.content.flatMap(_.storeIndex).filter(_.hrefs.nonEmpty).foreach: index =>
          hops = hops ++ resolveStore(page, index)
      case _ =>

    selectorHopsVar = hops
    pagesVar = pagesVar.filterNot(isHopPage)
    pages.foreach:
      case page: MarkupPage if page.source.exists(_.markup == TeiMarkup) =>
        page.content.flatMap(_.storeIndex).filter(_.hrefs.nonEmpty).foreach: index =>
          reportUnlisted(page, index)
      case _ =>

  private def resolveStore(page: MarkupPage, index: StoreIndex): Set[Seq[String]] =
    val sourcePath: Path = page.sourcePath.get
    val indexed: Seq[String] = sourcePath.path
    val children: List[Page] = index.hrefs.toList.flatMap: href =>
      val resolved: Path = sourcePath.resolveFrom(href)
      findBySource(resolved) match
        case None =>
          site.error(sourcePath, PageError.Unresolved, s"unresolved store include '$href'")
          None
        case Some(child) =>
          Some(child)

    page match
      case directory: DirectoryPage => directory.setStoreChildren(children)
      case _ =>

    index.hrefs.flatMap(href => hopDirectories(indexed, sourcePath.resolveFrom(href).path)).toSet

  private def hopDirectories(indexed: Seq[String], target: Seq[String]): Set[Seq[String]] =
    if !target.startsWith(indexed) then Set.empty
    else
      val between: Seq[String] = target.drop(indexed.length).dropRight(1)
      between.indices.map(i => indexed ++ between.take(i + 1)).toSet.filterNot: dir =>
        get(Path(dir :+ DirectoryPage.fileName *).html).exists(_.source.isDefined)

  private def isHopPage(page: Page): Boolean =
    page.isDirectory && page.source.isEmpty && selectorHopsVar.contains(page.path.path.init)

  private def reportUnlisted(page: MarkupPage, index: StoreIndex): Unit =
    val sourcePath: Path = page.sourcePath.get
    val indexed: Seq[String] = sourcePath.path
    val listed: Set[Path] = index.hrefs.map(sourcePath.resolveFrom).toSet

    pages.foreach: extra =>
      extra.sourcePath.foreach: extraSource =>
        if isUnlisted(extraSource, sourcePath, indexed, listed) then
          site.error(
            extraSource,
            PageError.NotInStore,
            s"not listed in store $sourcePath"
          )

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

  private def findBySource(sourcePath: Path): Option[Page] =
    pages.find(_.sourcePath.contains(sourcePath))

  // TODO search only pages corresponding to the 'kind'
  def find(
    path: Path,
    isAbsolute: Boolean,
    kind: Option[LinkKind]
  ): Option[Page] =
    // Exact path first so `/P/index.html` is the chunked TOC, not title-walked to `/P.html`.
    get(path)
      .orElse(if path.extension.isEmpty then get(path.html) else None)
      .orElse(pages.flatMap(page => is(page, path, isAbsolute)).headOption)

  def findByFileName(fileName: String, extension: Option[String]): Seq[Page] =
    pages.filter(page => page.path.fileName == fileName && page.path.extension == extension)

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

    if !isExtension(page.path, path) || path.path.isEmpty then None
    else loop(page, path.path)

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
