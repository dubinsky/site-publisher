package org.podval.tools.publish.site

import org.podval.metadata.Language
import org.podval.tools.publish.js.JSLibrary
import org.podval.tools.publish.markup.{AsciiDocMarkup, Link, TeiDate}
import org.podval.tools.publish.prose.ProseOptions
import org.podval.tools.publish.page.{EmbeddedAsset, MarkupPage, NamedWindows, PdfPage}
import org.podval.tools.publish.util.{Files, Git, Http, Icon, Logging, Media, ObsidianConfig, SiteOptions}
import org.podval.xml.Xml
import org.podval.xml.dsl.{*, given}
import com.sun.net.httpserver.HttpServer
import com.microsoft.playwright.{Browser, Playwright}
import org.slf4j.{Logger, LoggerFactory}
import java.io.File
import java.net.{URI, URISyntaxException}
import java.time.{Instant, OffsetDateTime, ZoneId}
import scala.util.Try

final class Site(options: SiteOptions) extends JSLibrary:
  // Site itself is a JavaScript library too
  override def cdn: String = ""
  override def stylesheet: Some[String] = Some(EmbeddedAsset.mainStyleSheet)

  // In <head> so it runs even when later markup is ill-formed and swallows body scripts.
  override def headInlineJs: Some[String] = Some(Site.siteSettingsJs)
  
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
    link(rel := "license", titleAttr := license, config.licenseLink.map(licenseLink => href := licenseLink))

  lazy val favicon: Option[Xml.Element] =
    for
      favicon <- config.favicon
      (name, extension) = Files.nameAndExtension(favicon)
      extension <- extension
      if Media.isImage(extension)
    yield
      link(rel:="icon", href:=s"/$favicon", `type`:=s"image/$extension")

  // Social links
  private val socialLinks: Seq[SocialLink] = Seq(
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

    // After throwIfErrors: a missing category hub is an unresolved link, not a failed build.
    categories.resolve(this)

    // Gather back-links from authored trees. Store / entity-lists `xml` is an empty
    // root, so generated index → entity hrefs are not backlinks.
    for
      page <- pages.pages.flatMap(_.asFullMarkupPage)
      content <- page.content
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
              from = page,
              ids = content.ids,
              inTeiHeader = element.getId.exists(headerIds.contains)
            )
        )
      )

    for
      page <- pages.pages.flatMap(_.asFullMarkupPage)
    do
      transclusions.add(transclusions.harvest(page))

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

  def siteHeader(page: MarkupPage): Xml.Element =
    val browse: Seq[Xml.Element] = Seq(
      page.up.flatMap(up => Option.when(up.up.isDefined)(up.navRef(Icon.arrowUp))),
      page.prev.map(_.navRef(Icon.arrowLeft)),
      page.next.map(_.navRef(Icon.arrowRight))
    ).flatten
    val views: Seq[Xml.Element] = page.formatLinks ++ page.translationLinks
    header(className := "site-header",
      div(className := "wrapper",
        a(
          className := "site-title",
          href := "/",
          rel := "author",
          NamedWindows.hierarchyTarget(config).map(name => target := name),
          config.title
        ),
        Xml.element("button")
          .addClass("nav-toggle")
          .set("type", "button")
          .set("aria-expanded", "false")
          .set("aria-controls", "site-nav")
          .set("aria-label", "Menu")
          .setChildren(Seq(span(className := "nav-toggle-icon"))),
        nav(id := "site-nav", className := "site-nav",
          navGroup("nav-pages", pages.headerPages.map(_.ref())),
          navGroup("nav-browse", browse),
          navGroup("nav-views", views)
        ),
        details(className := "site-settings",
          summary(
            className := "site-settings-toggle",
            titleAttr := "Settings",
            aria("label") := "Settings",
            Icon.gear.html
          ),
          div(className := "site-settings-menu", role := "group", aria("label") := "Settings",
            label(className := "site-settings-item",
              input(
                `type` := "checkbox",
                id := "setting-glossary-expand",
                attr("data-setting") := "glossary-expand"
              ),
              "Show glossary definitions in the text"
            ),
            label(className := "site-settings-item",
              input(
                `type` := "checkbox",
                id := "setting-transclusion-clean",
                attr("data-setting") := "transclusion-clean"
              ),
              "Seamless transclusions"
            )
          )
        )
      )
    )

  private def navGroup(cls: String, items: Seq[Xml.Element]): Option[Xml.Element] =
    Option.when(items.nonEmpty)(div(className := s"nav-group $cls", items))

  def siteFooter: Xml.Element =
    footer(className := "site-footer",
      div(className := "wrapper",
        h2(className := "footer-heading", config.title),
        div(className := "footer-col-wrapper",
          div(className := "footer-col",
            ul(className := "contact-list",
              li(config.author),
              li(a(href := s"mailto:${config.email}", config.email))
            )
          ),
          div(className := "footer-col",
            div(className := "social-links",
              ul(className := "social-media-list", socialLinks.map(social =>
                li(a(
                  rel := "me",
                  href := social.href,
                  target := "_blank",
                  titleAttr := social.title,
                  Icon.brand(social.icon).html,
                  span(className := "username", social.userName)
                ))
              ))
            )
          ),
          div(className := "footer-col",
            p(config.description),
            p(Feed.feedFooter)
          )
        )
      )
    )

object Site:
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

