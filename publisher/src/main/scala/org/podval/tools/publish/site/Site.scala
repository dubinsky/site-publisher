package org.podval.tools.publish.site

import org.podval.metadata.{Language, Term, TermCatalog}
import org.podval.tools.publish.js.HeadContribution
import org.podval.tools.publish.markup.{AsciiDocMarkup, Link, TeiDate}
import org.podval.tools.publish.prose.ProseOptions
import org.podval.tools.publish.page.{AuthoredHarvest, EmbeddedAsset, MarkupPage, PdfPage, SiteChrome}
import org.podval.tools.publish.util.{Files, Git, Http, Logging, Media, ObsidianConfig, SiteOptions}
import org.podval.xml.{Xml, XmlParser}
import org.podval.xml.dsl.{*, given}
import com.sun.net.httpserver.HttpServer
import com.microsoft.playwright.{Browser, Playwright}
import org.slf4j.{Logger, LoggerFactory}
import java.io.File
import java.net.{URI, URISyntaxException}
import java.time.{Instant, OffsetDateTime, ZoneId}
import scala.util.Try

final class Site(options: SiteOptions) extends HeadContribution:
  override def styleLinks: List[Xml.Element] =
    List(link(rel := "stylesheet", href := EmbeddedAsset.mainStyleSheet))

  // In <head> so it runs even when later markup is ill-formed and swallows body scripts.
  override def headScripts: List[Xml.Element] =
    List(script().inlineJs(Site.siteSettingsJs))
  
  // Directories
  val proseOptions: ProseOptions = ProseOptions(options.width, options.sentencePerLine)

  val sourceDirectory: File = File(options.sourceDirectoryPath).getAbsoluteFile
  Files.requireExists(sourceDirectory)
  Files.requireDirectory(sourceDirectory)

  def sourceFile(sourcePath: Path): File = sourcePath.file(sourceDirectory)

  // Absolute `--target-directory-name` is used as-is: `File(parent, child)` on Unix
  // would otherwise treat `/abs` as a path under the source tree.
  val targetDirectory: File =
    val named: File = File(options.targetDirectoryName)
    (if named.isAbsolute then named else File(sourceDirectory, options.targetDirectoryName)).getAbsoluteFile
  if targetDirectory.exists() then Files.requireDirectory(targetDirectory)

  // Posts and daily notes directories
  private val obsidianConfig: ObsidianConfig = ObsidianConfig(sourceDirectory)
  def postsDirectoryName: String = "_posts"
  val draftsDirectoryName: Option[String] = options.draftsDirectoryName
  def dailyNotesDirectoryName: Option[String] = obsidianConfig.daysFolder

  // Configuration
  private val configFile: File = File(sourceDirectory, "_site_config.yml")
  Files.requireExists(configFile)
  Files.requireFile(configFile)

  val config: Config = Config.decode(Files.read(configFile)) match
    case Left(error) => throw IllegalArgumentException(s"Malformed Config: ${error.getMessage}", error)
    case Right(result) => result

  /** IANA zone from `timezone`, or the JVM default when it is omitted or invalid. */
  val zone: ZoneId =
    config.timezone.flatMap(name => Try(ZoneId.of(name)).toOption).getOrElse(ZoneId.systemDefault)

  def toOffsetDateTime(instant: Instant): OffsetDateTime = instant.atZone(zone).toOffsetDateTime

  val languageSpec: Language.Spec = Site.languageSpec(config.lang)

  val teiDefaultCalendarIsJulian: Boolean = TeiDate.defaultIsJulian(config.teiDefaultCalendar)

  val uri: URI = URI(config.url)

  // Both hosts must be present: `mailto:` and a scheme-less site `url` both have host null.
  def sameSiteHost(uri: URI): Boolean =
    val hrefHost: String = uri.getHost
    val siteHost: String = this.uri.getHost
    hrefHost != null && siteHost != null && hrefHost.equalsIgnoreCase(siteHost)

  def isInternalLink(
    href: String,
    errorReporter: PageErrorReporter
  ): Boolean =
    try
      val uri: URI = URI(href)
      if sameSiteHost(uri) then errorReporter.error(PageError.SelfLink, href)
      uri.getScheme == null
    catch case e: URISyntaxException => true

  // Terms before pages: store axes and catalog titles resolve while pages load.
  val terms: TermCatalog = Site.loadTerms(sourceDirectory)

  // Components
  val pages: Pages = Pages(this)
  val ignore: Ignore = Ignore(this)
  val git: Git = Git(sourceDirectory)
  val backLinks: BackLinks = BackLinks()
  val transclusions: TransclusionEdges = TransclusionEdges()
  val categories: Categories = Categories()
  val tags: Tags = Tags(this)
  val posts: Posts = Posts(this)
  val externalLinks: ExternalLinks = ExternalLinks(this)

  // Errors
  val errors: Errors = Errors(this, treatErrorsAsWarnings = options.treatErrorsAsWarnings)

  // One pass at the end of load. siteHeader and the html attribute both read it.
  private var readerFeaturesVar: ReaderFeatures = ReaderFeatures.empty
  private[publish] def readerFeatures: ReaderFeatures = readerFeaturesVar

  def error(
    sourcePath: Path,
    kind: PageError.Kind,
    message: String,
    cause: Option[Throwable] = None
  ): Unit = errors.error(PageError(sourcePath, kind, message, cause))

  // Logging
  Logging.configureLogBack(level = options.logLevel)
  val log: Logger = LoggerFactory.getLogger(this.getClass)

  log.info(s"source directory: $sourceDirectory")
  log.info(s"target directory: $targetDirectory")
  log.info(s"configuration file: $configFile")
  log.debug(s"configuration:\n" + Config.encodeToString(config))
  log.debug(s"ignore rules:\n" + ignore.rules)

  // Google Analytics
  val googleAnalytics: Option[String] = if !options.production then None else config.googleAnalytics

  lazy val license: Option[Xml.Element] = config.license.map: license =>
    link(
      rel := "license",
      titleAttr := license.name.trim,
      license.link.map(url => href := url.trim)
    )

  lazy val favicon: Option[Xml.Element] =
    for
      favicon <- config.favicon
      (name, extension) = Files.nameAndExtension(favicon)
      extension <- extension
      if Media.isImage(extension)
    yield
      link(rel:="icon", href:=s"/$favicon", `type`:=s"image/$extension")

  // Social links
  private[publish] val socialLinks: Seq[SocialLink] = Seq(
    config.social.github.map(SocialLink.GitHub(_)),
    config.social.twitter.map(SocialLink.Twitter(_)),
    config.social.linkedin.map(SocialLink.LinkedIn(_))
  ).flatten

  def prettyPrint(): Unit = PrettyPrint.run(this)

  def generate(): Unit =
    try
      loadAndGenerate()
    finally
      stopHttpServer()

  def serve(): Unit =
    loadAndGenerate()
    log.info(s"Serving $targetDirectory on port $httpServerPort")

  // Note: I do not see any reason to bother with generating into a temporary directory and then renaming it.
  private def loadAndGenerate(): Unit =
    try
      load()

      if config.checkLinks then log.info("Checking external links")

      // Wipe out output directory
      Files.deleteDirectory(targetDirectory)

      // PDFs print via HTTP from already-written HTML/assets; write them last
      // so any assets used are already written.
      // Errors is written after other HTML so diagnostics from rendering
      // (unknown citations, unresolved links) appear on the Errors page.
      val (pdfPages, otherPages) = pages.pages.partition:
        case _: PdfPage => true
        case _ => false
      val (errorPages, rest) = otherPages.partition:
        case _: Errors => true
        case _ => false
      (rest ++ errorPages ++ pdfPages).foreach: page =>
        log.debug(s"Writing ${page.path}")
        page.write()

      CollectionAliases.write(targetDirectory, pages)

      // Done
      log.info("Done generating!")
    finally
      stopConverters()

  private def load(): Unit =
    // Load all pages
    pages.load()

    // After load, before backlinks. The soft reference can still miss during the scan.
    pages.pages.foreach: page =>
      page.source.foreach(_.pin())
      page match
        case markup: MarkupPage => markup.inlineListProse.foreach(_.pin())
        case _ =>

    // After throwIfErrors: a missing category hub is an unresolved link, not a failed build.
    categories.resolve(this)

    // Gather back-links from authored trees. Store / entity-lists `xml` is an empty
    // root, so generated index → entity hrefs are not backlinks.
    // Absorbed authored prose and inlineListProse are harvested; the empty store /
    // entity-lists root is why generated index hrefs are not.
    for
      case markup: MarkupPage <- pages.pages
      content <- AuthoredHarvest.linkContents(markup)
    do
      // Ids inside teiHeader are absent from the published page: the document-header
      // table is built again from the raw header and does not keep them.
      val headerIds: Set[String] = content.xml
        .gather(element => Option.when(element.isNamed("teiHeader"))(element))
        .flatMap(_.gather(_.getId))
        .toSet
      backLinks.addBackLinks(
        content.xml.gatherWithParent(
          gatherElement = (element: Xml.Element, parent: Option[Xml.Element]) =>
            if !Link.isInternal(element) then None else BackLink(
              element,
              parent = parent.get,
              from = markup,
              ids = content.ids,
              inTeiHeader = element.getId.exists(headerIds.contains)
            )
        )
      )

    for
      case markup: MarkupPage <- pages.pages
      content <- AuthoredHarvest.linkContents(markup)
    do
      transclusions.add(transclusions.harvest(markup, content))

    val detected: ReaderFeatures = ReaderFeatures.detect(this)
    readerFeaturesVar = detected
    log.debug(
      s"reader features: glossary=${detected.glossary} citation=${detected.citation} " +
      s"choice=${detected.choice} transclusion=${detected.transclusion} facsimile=${detected.facsimile} " +
      s"add=${detected.apparatusAdd} del=${detected.apparatusDel} see=${detected.apparatusSee} " +
      s"kinds=${detected.apparatus.mkString(" ")}"
    )

  private var playwrightVar: Option[Playwright] = None
  def playwright: Playwright = synchronized:
    playwrightVar.getOrElse:
      val result: Playwright = PdfPage.playwright
      playwrightVar = Some(result)
      result

  private var browserVar: Option[Browser] = None
  def browser: Browser = synchronized:
    browserVar.getOrElse:
      val result: Browser = PdfPage.browser(playwright)
      browserVar = Some(result)
      result

  private def stopConverters(): Unit =
    AsciiDocMarkup.closeAsciidoctor()
    browserVar.foreach(_.close())
    playwrightVar.foreach(_.close())

  private var httpServerVar: Option[HttpServer] = None

  private def httpServer: HttpServer = synchronized:
    httpServerVar.getOrElse:
      val result: HttpServer = Http.start(
        targetDirectory,
        raw => pages.rewriteRequest(Path.fromHref(raw)).map(_.toString)
      )
      val port: Int = result.getAddress.getPort
      if port != Http.defaultPort then
        log.info(s"Port ${Http.defaultPort} in use; HTTP server on $port")
      httpServerVar = Some(result)
      result

  def httpServerPort: Int = httpServer.getAddress.getPort

  private def stopHttpServer(): Unit =
    httpServerVar.foreach(_.stop(0))

  def siteHeader(page: MarkupPage): Xml.Element = SiteChrome.header(this, page)

  def siteFooter: Xml.Element = SiteChrome.footer(this)

  def siteFooter(page: MarkupPage): Xml.Element = SiteChrome.footer(this, page)

object Site:
  /** Built-in catalog, plus `{source}/_terms.xml` when that file exists. A bad file throws. */
  private[site] def loadTerms(directory: File): TermCatalog =
    val file: File = File(directory, "_terms.xml")
    if !file.isFile then Terms.catalog
    else
      val root: Xml.Element = XmlParser.parseXml(Files.read(file)).fold(error => throw error, identity)
      val extra: Seq[Term] = Term.codec.decodeCatalog(root, "Terms").fold(error => throw error, identity)
      Terms.catalog.merge(extra)

  /** `lang` from `_site_config.yml` (`en`, `ru`, `en-US`, `Russian`, …). Omitted or unknown is English. */
  def languageSpec(lang: Option[String]): Language.Spec =
    lang.map(_.trim).filter(_.nonEmpty).flatMap(parseLanguage).getOrElse(Language.English).toSpec

  private def parseLanguage(lang: String): Option[Language] =
    val primary: String = lang.takeWhile(c => c != '-' && c != '_')
    Language.forName(lang).orElse(Language.forName(primary))

  private lazy val siteSettingsJs: String =
    Files.readResource("/org/podval/tools/publish/site/siteSettings.js")

  def main(args: Array[String]): Unit =
    val options: SiteOptions = SiteOptions.forArgs(args)
    rejectPrettyPrintWithServe(options)
    val site: Site = Site(options)
    if options.prettyPrint then site.prettyPrint()
    else if options.serve then site.serve()
    else site.generate()

  def rejectPrettyPrintWithServe(options: SiteOptions): Unit =
    if options.prettyPrint && options.serve then
      throw IllegalArgumentException("--pretty-print does not generate or serve")
