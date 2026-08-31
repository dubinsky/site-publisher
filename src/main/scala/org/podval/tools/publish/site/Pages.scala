package org.podval.tools.publish.site

import org.podval.tools.publish.markup.{AssetRef, EntityKind, Facsimile, Link, LinkKind, Markup, TeiMarkup, XmlMarkup}
import org.podval.tools.publish.page.{Alias, AssetWithSourcePath, CollectionIndex, DirectoryPage, EmbeddedAsset,
  EntityListPage, EntityLists, FacsimilePage, FrontMatter, MarkupPage, Page, PageContent, PageSource, PdfPage,
  SimpleMarkupPage, StoreIndexPage, StoreIndexes}
import org.podval.tools.publish.util.{Files, Media, Strings}
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

  def facsimilePage(page: Page): Option[FacsimilePage] =
    val original: Page = Facsimile.original(page)
    pages.collectFirst:
      case viewer: FacsimilePage if viewer.document.path == original.path => viewer

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

    resolveStores()
    addStoreIndexes()
    installHome()
    installCollectionAliases()
    headerPagesVar = resolveHeaderPages()

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

    indexEntities()
    resolveEntityLists()
    
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

  private def installCollectionAliases(): Unit =
    pages.foreach: page =>
      page.store.flatMap(_.alias).foreach: name =>
        val short: Path = Path.fromHref(name)
        val source: Path = page.sourcePath.getOrElse(page.path)
        if short.path.isEmpty then
          site.error(source, PageError.Unresolved, s"collection alias is empty")
        else
          val key: Seq[String] = short.path
          aliasByPrefix.get(key) match
            case Some(existing) if existing.real == page.real =>
              ()
            case Some(existing) =>
              site.error(
                source,
                PageError.Duplicate,
                s"collection alias '$name' collides with $existing"
              )
            case None =>
              get(short.html) match
                case Some(other) if other.real == page.real =>
                  aliasByPrefix = aliasByPrefix.updated(key, other match
                    case alias: Alias => alias
                    case _ => new Alias(site, page.real, short.html)
                  )
                case Some(other) =>
                  site.error(
                    source,
                    PageError.Duplicate,
                    s"collection alias '$name' collides with $other"
                  )
                case None =>
                  aliasByPrefix = aliasByPrefix.updated(key, new Alias(site, page.real, short.html))

  private var aliasByPrefix: Map[Seq[String], Alias] = Map.empty

  /** Store/directory alias prefixes for the Worker table (`from` → collection directory `to`). */
  def collectionAliasEntries: Seq[CollectionAliases.Entry] =
    aliasByPrefix.toSeq.flatMap: (from, alias) =>
      aliasTargetDirectory(alias.real).map: to =>
        CollectionAliases.Entry(from, to, alias.real.path)

  private def aliasTargetDirectory(page: Page): Option[Seq[String]] =
    aliasDirectory(page).orElse:
      Option.when(page.isInstanceOf[StoreIndexPage])(page.path.withoutHtml.path)

  /** Public href for `page`: longest directory/store alias prefix, else the written path. */
  def publishedPath(page: Page): Path = publishedPath(page.real.path)

  def publishedPath(path: Path): Path =
    val realSegs: Seq[String] = path.withoutHtml.path
    val hits: Seq[Path] = aliasByPrefix.iterator.toSeq.flatMap: (short, alias) =>
      aliasDirectory(alias.real).flatMap: dir =>
        val targetSegs: Seq[String] = alias.real.path.withoutHtml.path
        if realSegs == targetSegs then Some(Path(short, path.extension))
        else Option.when(realSegs.startsWith(dir) && realSegs.length > dir.length)(
          Path(short ++ realSegs.drop(dir.length), path.extension)
        )
    hits.minByOption(hit => (hit.path.length, hit.toString)).getOrElse(path)

  /** Map an inbound URL onto the written file path (Worker / local `serve()`).
    * Includes collection-alias prefix and collector `/alias/facsimile/P`. */
  def rewriteRequest(request: Path): Option[Path] =
    findViaAlias(request).map(_.real.path)

  private def add(page: Page): Unit =
    pagesVar = pagesVar.appended(page)
    page match
      case alias: Alias =>
        aliasByPrefix = aliasByPrefix.updated(alias.path.withoutHtml.path, alias)
      case _ =>
    // Add implied directories
    page.parent
    page.asFullMarkupPage.foreach: page =>
      page.aliases.foreach(add)
      if page.chunk then page.chunks.foreach(add)
      if page.pdf then add(PdfPage(page))
      if Facsimile.needed(page) then add(FacsimilePage(page))
      
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
    def isAuthoredDirectory(dir: Seq[String]): Boolean =
      get(Path(dir :+ DirectoryPage.fileName *).html).exists(_.source.isDefined)
    pages.foreach:
      case page: MarkupPage =>
        page.store.foreach: store =>
          if store.hrefs.nonEmpty then
            val (children, pageHops) = store.bind(page, findBySource, isAuthoredDirectory)
            val listed: List[Page] = CollectionIndex.listingChildren(store, children)
            store.setBoundChildren(listed)
            page match
              case directory: DirectoryPage =>
                directory.setStoreChildren(listed)
              case _ =>
            hops = hops ++ pageHops
          else if store.isCollection then
            page match
              case directory: DirectoryPage =>
                val originals: List[Page] = CollectionIndex.originalsUnder(directory)
                store.setBoundChildren(originals)
                directory.setStoreChildren(originals)
              case _ =>
      case _ =>

    selectorHopsVar = hops
    pagesVar = pagesVar.filterNot(isHopPage)
    pages.foreach:
      case page: MarkupPage =>
        page.store.filter(_.hrefs.nonEmpty).foreach: store =>
          store.reportUnlisted(page, pages)
      case _ =>

  private def isHopPage(page: Page): Boolean =
    page.isDirectory && page.source.isEmpty && selectorHopsVar.contains(page.path.path.init)

  private def findBySource(sourcePath: Path): Option[Page] =
    pages.find(_.sourcePath.contains(sourcePath))

  private def addStoreIndexes(): Unit =
    val roots: Seq[Page] = pages.filter(StoreIndexes.isRootStore)
    val trees: Seq[StoreIndexPage] = roots.map: root =>
      val tree: StoreIndexPage = StoreIndexPage.tree(site, root)
      add(tree)
      tree
    roots.foreach: root =>
      add(StoreIndexPage.flat(site, root))
    trees match
      case Seq(tree) =>
        installLeafAlias(StoreIndexes.collectionsAlias, tree)
      case _ =>

  private def installLeafAlias(name: String, target: Page): Unit =
    val short: Path = Path.fromHref(name)
    val key: Seq[String] = short.path
    if key.isEmpty then
      site.error(target.path, PageError.Unresolved, s"store index alias '$name' is empty")
    else
      aliasByPrefix.get(key) match
        case Some(existing) if existing.real == target.real =>
          ()
        case Some(existing) =>
          site.error(
            target.path,
            PageError.Duplicate,
            s"store index alias '$name' collides with $existing"
          )
        case None =>
          get(short.html) match
            case Some(other) if other.real == target.real =>
              aliasByPrefix = aliasByPrefix.updated(key, other match
                case alias: Alias => alias
                case _ => new Alias(site, target.real, short.html)
              )
            case Some(other) =>
              site.error(
                target.path,
                PageError.Duplicate,
                s"store index alias '$name' collides with $other"
              )
            case None =>
              aliasByPrefix = aliasByPrefix.updated(key, new Alias(site, target.real, short.html))

  // Entity refs (`persName` / `placeName` / `orgName` `@ref`) look up (kind, filename)
  // and do not title-walk. Other LinkKind values still fall through to path/title search.
  def find(
    path: Path,
    isAbsolute: Boolean,
    kind: Option[LinkKind]
  ): Option[Page] =
    kind match
      case Some(LinkKind.Entity(entityKind)) =>
        findEntity(path, entityKind)
      case _ =>
        // Exact path first so `/P/index.html` is the chunked TOC, not title-walked to `/P.html`.
        findExact(path)
          .orElse(findViaAlias(path))
          .orElse(findWalk(path, isAbsolute))

  private def findExact(path: Path): Option[Page] =
    get(path).orElse(if path.extension.isEmpty then get(path.html) else None)

  private def findWalk(path: Path, isAbsolute: Boolean): Option[Page] =
    pages.flatMap(page => is(page, path, isAbsolute)).headOption

  // `/short/child` is `child` under the page that permalink/alias `short` names, when that
  // page is a directory or a TEI `store`/`collection`. Exact `/short` is the target itself.
  // Does not recurse through `find` (the expanded path still starts with the alias prefix).
  private def findViaAlias(path: Path): Option[Page] =
    val segments: Seq[String] = path.path
    if segments.isEmpty then None
    else
      (segments.length until 0 by -1)
        .flatMap: prefixLen =>
          aliasByPrefix.get(segments.take(prefixLen)).flatMap: alias =>
            val remainder: Seq[String] = segments.drop(prefixLen)
            if remainder.isEmpty then Some(alias.real)
            else findUnderAliased(alias.real, remainder, path.extension)
        .headOption

  private def aliasDirectory(page: Page): Option[Seq[String]] =
    if page.isDirectory then Some(page.path.path.init)
    else if page.store.isDefined then Some(page.path.withoutHtml.path)
    else None

  private def findUnderAliased(
    real: Page,
    remainder: Seq[String],
    extension: Option[String]
  ): Option[Page] =
    if remainder.isEmpty then None
    else
      aliasDirectory(real).flatMap: dir =>
        def lookup(segs: Seq[String]): Option[Page] =
          val joined: Path = Path(Path.normalize(dir ++ segs), extension)
          findExact(joined)
            .orElse(findBySourceUnder(real, segs))
            .orElse(findFacsimileOf(real, dir, segs))
            .orElse(findWalk(joined, isAbsolute = true))
        Facsimile.inboundRemainder(remainder).flatMap(lookup).orElse(lookup(remainder))

  // Translation `/alias/facsimile/P-xx` has no viewer page; use the original's.
  private def findFacsimileOf(real: Page, dir: Seq[String], segs: Seq[String]): Option[Page] =
    segs match
      case Seq(name, Facsimile.fileName) =>
        findExact(Path(Path.normalize(dir :+ name)))
          .orElse(findBySourceUnder(real, Seq(name)))
          .flatMap(facsimilePage)
      case _ => None

  private def findBySourceUnder(real: Page, remainder: Seq[String]): Option[Page] =
    real.sourcePath.flatMap: source =>
      val directory: Seq[String] =
        if source.fileName == DirectoryPage.fileName then source.path.init else source.path
      val want: Seq[String] = directory ++ remainder
      pages.find(_.sourcePath.exists(_.path == want))

  private var entityByKindAndId: Map[(EntityKind, String), Page] = Map.empty

  private def indexEntities(): Unit =
    val grouped: Map[(EntityKind, String), List[Page]] = pages.flatMap: page =>
      for
        source <- page.source if source.markup == TeiMarkup
        kind <- page.entityKind
        sourcePath <- page.sourcePath
      yield (kind, sourcePath.fileName) -> page
    .groupMap(_._1)(_._2)

    entityByKindAndId = grouped.flatMap:
      case (key, List(page)) =>
        Some(key -> page)
      case ((kind, id), duplicates) =>
        duplicates.foreach: page =>
          site.error(
            page.sourcePath.getOrElse(page.path),
            PageError.Duplicate,
            s"duplicate ${kind.element} entity '$id'"
          )
        None

  private def findEntity(path: Path, entityKind: EntityKind): Option[Page] =
    if path.extension.nonEmpty || path.path.size != 1 then None
    else entityByKindAndId.get((entityKind, path.fileName))

  // path could be `name`, `path/name`(?) - or empty, for intrapage links.
  // fragment could be `#section`, `#section#subsection`, `#^block`, or #id.
  def resolve(
    ref: String,
    kind: Option[LinkKind],
    from: Page
  ): Option[Link] =
    val (pathStringRaw: String, fragmentStr: Option[String]) = Strings.splitFirst(ref, '#')
    val pathString: String = pathStringRaw.trim
    val isAbsolute: Boolean = pathString.startsWith("/")
    val isFileHref: Boolean = isAbsolute || Path.isRelativeFileHref(pathString)
    val path: Path =
      if pathString.isEmpty then Path.root
      else if isFileHref then from.path.resolveFrom(pathString)
      else Path.fromHref(pathString)

    val to: Option[Page] =
      if pathString.isEmpty
      then Some(from)
      else find(path, isFileHref, kind)

    to.map: to =>
      val fragment: Option[Link.ToFragment] = fragmentStr.flatMap: fragment =>
        val content: Option[PageContent] = to.real.content
        if fragment.startsWith("^")
        then content.flatMap(_.blocks.resolve(id = fragment.substring(1).trim))
        else if fragment.contains("#")
        then content.map(_.toc).flatMap(_.resolveSection(names = fragment.split('#').map(_.trim).toSeq))
        else content.flatMap(_.ids.resolve(fragment)).orElse(
          content.map(_.toc).flatMap(_.resolveSection(names = Seq(fragment.trim)))
        )

      Link(
        page = to,
        isIntrapage = from == to,
        fragment = fragment
      )

  def resolveAsset(
    element: Xml.Element,
    from: Page,
    errorReporter: PageErrorReporter,
    reportMissing: Boolean
  ): Xml.Element =
    AssetRef.resourceAttr(element).flatMap(attr =>
      element.get(attr).map(_.trim).filter(_.nonEmpty).map(attr -> _)
    ) match
      case None => element
      case Some((attr, ref)) =>
        val (pathStringRaw: String, fragment: Option[String]) = Strings.splitFirst(ref, '#')
        val pathString: String = pathStringRaw.trim
        val isWiki: Boolean = AssetRef.isWikiEmbed(element)
        val stripped: Xml.Element = AssetRef.clearWikiEmbed(element)
        if pathString.isEmpty || !from.site.isInternalLink(pathString, errorReporter) then stripped
        else if !isAssetPath(pathString) then stripped
        else lookupAsset(pathString, from, isWiki) match
          case None =>
            if reportMissing then
              errorReporter.error(PageError.MissingAsset, s"missing asset '$ref'")
            stripped.add(AssetRef.UnresolvedClass)
          case Some(page) =>
            val url: String = page.path.toString + fragment.fold("")(f => s"#$f")
            stripped.set(attr, url)

  private def isAssetPath(pathString: String): Boolean =
    Files.nameAndExtension(
      pathString.split('/').map(_.trim).filterNot(_.isEmpty).lastOption.getOrElse(pathString)
    )._2.exists(Media.isAsset)

  private def lookupAsset(pathString: String, from: Page, isWiki: Boolean): Option[Page] =
    val path: Path =
      if isWiki && !pathString.startsWith("/") && pathString.contains("/")
      then from.path.resolveFrom("/" + pathString)
      else from.path.resolveFrom(pathString)
    find(path, isAbsolute = true, kind = None).orElse:
      if !isWiki || pathString.contains('/') then None
      else
        val parsed: Path = Path.fromHref(pathString)
        findByFileName(parsed.fileName, parsed.extension) match
          case Seq(one) => Some(one)
          case _ => None

  private def resolveEntityLists(): Unit =
    pages.foreach:
      case directory: DirectoryPage =>
        directory.doc.flatMap(_.asEntityLists).foreach: lists =>
          val entities: Seq[Page] = EntityLists.entitiesUnder(directory)
          directory.setStoreChildren(
            entities.sortBy(page => page.sourcePath.map(_.fileName).getOrElse(page.path.fileName)).toList
          )
          val listPages: List[EntityListPage] = lists.listPages(directory, get)
          listPages.foreach(add)
          listPages.foreach(_.setSiblings(listPages))
      case _ =>

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
