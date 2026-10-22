package org.podval.tools.publish.site

import org.podval.metadata.{Language, Term, TermCatalog}
import org.podval.tools.publish.js.JSLibrary
import org.podval.tools.publish.markup.{AsciiDocMarkup, Facsimile, Glossary, Link, LinkKind, TeiDate, Transclusion, WikiLink}
import org.podval.tools.publish.prose.ProseOptions
import org.podval.tools.publish.page.{EmbeddedAsset, FullMarkupPage, MarkupPage, NamedWindows, PageContent, PdfPage}
import org.podval.tools.publish.util.{Files, Git, Http, Icon, Logging, Media, ObsidianConfig, SiteOptions}
import org.podval.xml.{Xml, XmlElement, XmlParser}
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

    val detected: ReaderFeatures = detectReaderFeatures()
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

  private def detectReaderFeatures(): ReaderFeatures =
    var glossary: Boolean = false
    var citation: Boolean = false
    var choice: Boolean = false
    var transclusion: Boolean = transclusions.all.exists(_.toPage.isDefined)
    var facsimile: Boolean = false
    var apparatusAdd: Boolean = false
    var apparatusDel: Boolean = false
    var apparatusSee: Boolean = false

    def visitTree(
      xml: Xml.Element,
      from: MarkupPage,
      host: Option[PageContent],
      transclusionsToo: Boolean,
      dropHeader: Boolean
    ): Unit =
      // Same drop as DocumentContent.selectedXml: a collection document does not publish teiHeader.
      val dropDirectHeader: Boolean =
        dropHeader && from.parent.exists(_.store.exists(_.isCollection)) && xml.isNamed("TEI")
      def visit(element: Xml.Element, dropChildHeader: Boolean): Unit =
        if !element.isNamed(XmlElement.Code.localName) then
          if element.isNamed("choice") then choice = true
          if element.isElement(XmlElement.Span) then element.get("data-apparatus") match
            case Some("add") => apparatusAdd = true
            case Some("del") => apparatusDel = true
            case Some("see") => apparatusSee = true
            case _ =>
          if !citation && host.nonEmpty && Link.isInternal(element) && !WikiLink.isTranscluded(element) then
            element.getHref.foreach: href =>
              pages.resolve(href, LinkKind.of(element), from).foreach: link =>
                val contentOf: Option[PageContent] =
                  if link.isIntrapage then host else link.page.content
                val gloss: Boolean = link.fragment.exists: fragment =>
                  contentOf.exists(_.glossaryDefinitions.contains(fragment.id))
                val bibl: Boolean = link.fragment.exists: fragment =>
                  contentOf.exists(_.bibliographyDefinitions.contains(fragment.id))
                if !gloss && bibl then citation = true
          if transclusionsToo && !transclusion && WikiLink.isTranscluded(element) then
            element.getHref.filterNot(Transclusion.isObsidianBase).foreach: href =>
              pages.resolve(href, LinkKind.of(element), from).foreach: link =>
                Transclusion.authoredSource(link, href) match
                  case Transclusion.Source.Ok(_, _, _) => transclusion = true
                  case _ =>
          element.childElements.foreach: child =>
            if !(dropChildHeader && child.isNamed("teiHeader")) then visit(child, dropChildHeader = false)
      visit(xml, dropDirectHeader)

    // Harvest strips footnote bodies from the tree; render appends them.
    // A collection document drops teiHeader and publishes these cells instead.
    def scanPrepared(content: PageContent, from: MarkupPage, transclusionsToo: Boolean): Unit =
      if content.glossaryDefinitions.nonEmpty then glossary = true
      visitTree(content.xml, from, Some(content), transclusionsToo, dropHeader = true)
      content.footnotes.values.foreach: footnote =>
        footnote.nodes.foreach: node =>
          node.asElement.foreach: element =>
            if Glossary.definitions(element).nonEmpty then glossary = true
            visitTree(element, from, Some(content), transclusionsToo = false, dropHeader = false)
      if from.parent.exists(_.store.exists(_.isCollection)) then
        content.doc.documentHeader.foreach: header =>
          (header.description.toSeq ++ header.authors ++ header.addressee.toSeq ++
            header.transcribers ++ header.translators).foreach: element =>
            visitTree(element, from, None, transclusionsToo = false, dropHeader = false)

    def scanPage(page: MarkupPage, transclusionsToo: Boolean): Unit =
      page.content.foreach: content =>
        scanPrepared(content, page, transclusionsToo)
      page.inlineListProse.foreach: source =>
        scanPrepared(source.content, source.page, transclusionsToo = true)
      page.store.foreach: store =>
        (store.title.toSeq ++ store.description.toSeq ++ store.body.toSeq).foreach: fragment =>
          visitTree(fragment, page, None, transclusionsToo = false, dropHeader = false)

    pages.pages.foreach:
      case page: FullMarkupPage =>
        if Facsimile.needed(page) then facsimile = true
        scanPage(page, transclusionsToo = false)
      case page: MarkupPage =>
        scanPage(page, transclusionsToo = true)
      case _ =>

    ReaderFeatures(glossary, citation, choice, transclusion, facsimile, apparatusAdd, apparatusDel, apparatusSee)

  def siteHeader(page: MarkupPage): Xml.Element =
    given Language.Spec = languageSpec
    val features: ReaderFeatures = readerFeatures
    val browse: Seq[Xml.Element] = Seq(
      page.up.flatMap(up => Option.when(up.up.isDefined)(up.navRef(Icon.arrowUp))),
      page.prev.map(_.navRef(Icon.arrowLeft)),
      page.next.map(_.navRef(Icon.arrowRight))
    ).flatten
    val views: Seq[Xml.Element] = page.formatLinks ++ page.translationLinks ++ page.readingLinks
    page.withInterfaceLang(header(className := "site-header",
      div(className := "wrapper",
        a(
          className := "site-title",
          href := "/",
          rel := "author",
          NamedWindows.hierarchyTarget(config).map(name => target := name),
          config.title
        ),
        Option.when(config.search.enabled)(headerSearch),
        Xml.element("button")
          .addClass("nav-toggle")
          .set("type", "button")
          .set("aria-expanded", "false")
          .set("aria-controls", "site-nav")
          .set("aria-label", Messages.text("nav.menu"))
          .setChildren(Seq(span(className := "nav-toggle-icon"))),
        nav(id := "site-nav", className := "site-nav",
          navGroup("nav-pages", pages.headerPages.map(_.ref())),
          navGroup("nav-browse", browse),
          navGroup("nav-views", views)
        ),
        details(className := "site-settings",
          summary(
            className := "site-settings-toggle",
            titleAttr := Messages.text("nav.settings"),
            aria("label") := Messages.text("nav.settings"),
            Icon.gear.html
          ),
          div(className := "site-settings-menu", role := "group", aria("label") := Messages.text("nav.settings"),
            div(className := "site-settings-section", colorSchemeSetting),
            Option.when(features.inText)(
              div(
                className := "site-settings-section",
                role := "group",
                aria("labelledby") := "setting-in-text-label",
                div(
                  className := "site-settings-heading",
                  id := "setting-in-text-label",
                  Messages.text("settings.in-text")
                ),
                Option.when(features.terms)(
                  label(className := "site-settings-item",
                    input(
                      `type` := "checkbox",
                      id := "setting-glossary-expand",
                      attr("data-setting") := "glossary-expand"
                    ),
                    Messages.text("settings.glossary")
                  )
                ),
                Option.when(features.choice)(
                  label(className := "site-settings-item",
                    input(
                      `type` := "checkbox",
                      id := "setting-abbr-expand",
                      attr("data-setting") := "abbr-expand",
                      attr("checked") := "checked"
                    ),
                    Messages.text("settings.editorial")
                  )
                )
              )
            ),
            Option.when(features.layout)(
              div(className := "site-settings-section",
                Option.when(features.transclusion)(
                  label(className := "site-settings-item",
                    input(
                      `type` := "checkbox",
                      id := "setting-transclusion-clean",
                      attr("data-setting") := "transclusion-clean"
                    ),
                    Messages.text("settings.transclusion")
                  )
                ),
                Option.when(features.facsimile)(
                  label(className := "site-settings-item",
                    input(
                      `type` := "checkbox",
                      id := "setting-facsimile-aside",
                      attr("data-setting") := "facsimile-aside",
                      attr("checked") := "checked"
                    ),
                    Messages.text("settings.facsimile")
                  )
                )
              )
            ),
            Option.when(features.apparatus.nonEmpty)(
              div(
                className := "site-settings-section",
                features.apparatus.map(kind => apparatusSetting(kind, Messages.text(s"settings.$kind")))
              )
            )
          )
        )
      )
    ))

  private def headerSearch: Xml.Element =
    given Language.Spec = languageSpec
    val accessible: String = Messages.text("nav.search")
    element(
      "form",
      className := "header-search",
      attr("action") := "/search",
      attr("method") := "get",
      role := "search",
      element(
        "button",
        `type` := "submit",
        aria("label") := accessible,
        Icon("magnifying-glass", Icon.Solid).html
      ),
      input(
        `type` := "search",
        attr("name") := "q",
        aria("label") := accessible
      )
    )

  private def colorSchemeSetting: Xml.Element =
    given Language.Spec = languageSpec
    div(
      className := "site-settings-choice",
      role := "radiogroup",
      aria("labelledby") := "setting-color-scheme-label",
      span(id := "setting-color-scheme-label", className := "site-settings-choice-label", Messages.text("settings.color")),
      div(
        className := "site-settings-radios",
        colorChoice("system", Messages.text("settings.system"), selected = true),
        colorChoice("light", Messages.text("settings.light"), selected = false),
        colorChoice("dark", Messages.text("settings.dark"), selected = false)
      )
    )

  private def colorChoice(value: String, text: String, selected: Boolean): Xml.Element =
    label(
      input(
        `type` := "radio",
        attr("name") := "color-scheme",
        attr("value") := value
      ).when(selected)(attr("checked") := "checked"),
      text
    )

  private def apparatusSetting(kind: String, groupLabel: String): Xml.Element =
    val setting: String = s"apparatus-$kind"
    div(
      className := "site-settings-choice",
      role := "radiogroup",
      // Not `data-apparatus`: that attribute is the prose mark, and those rules would wrap this group.
      attr("data-apparatus-kind") := kind,
      aria("labelledby") := s"setting-$setting-label",
      span(id := s"setting-$setting-label", className := "site-settings-choice-label", groupLabel),
      div(
        className := "site-settings-radios",
        Seq("distinct", "plain", "hidden").map: value =>
          label(
            input(
              `type` := "radio",
              attr("name") := setting,
              attr("value") := value
            ).when(value == "distinct")(attr("checked") := "checked"),
            apparatusLabel(kind, value)
          )
      )
    )

  /** Distinct shows the marks the text uses. Additions `[ ]`, deletions struck through, sources `< >`. */
  private def apparatusLabel(kind: String, value: String): String =
    given Language.Spec = languageSpec
    value match
      case "plain" => Messages.text("settings.plain")
      case "hidden" => Messages.text("settings.hidden")
      case "distinct" => kind match
        case "add" => Messages.text("settings.distinct.add")
        case "del" => Messages.text("settings.distinct.del")
        case "see" => Messages.text("settings.distinct.see")
        case _ => "Distinct"
      case other => other.capitalize

  private def navGroup(cls: String, items: Seq[Xml.Element]): Option[Xml.Element] =
    Option.when(items.nonEmpty)(div(className := s"nav-group $cls", items))

  private def footerLicense(license: Config.License): Xml.Element =
    div(
      className := "footer-license",
      license.holder.map(holder =>
        license.holderLink match
          case Some(url) => a(href := url.trim, holder.trim)
          case None => span(holder.trim)
      ),
      license.link match
        case Some(url) => a(rel := "license", href := url.trim, license.name.trim)
        case None => span(license.name.trim)
    )

  def siteFooter: Xml.Element =
    given Language.Spec = languageSpec
    footer(className := "site-footer",
      div(className := "wrapper",
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
            div(className := "footer-about",
              div(className := "footer-description", config.description),
              Feed.feedFooter,
              config.license.map(footerLicense)
            )
          )
        )
      )
    )

  def siteFooter(page: MarkupPage): Xml.Element = page.withInterfaceLang(siteFooter)

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

private[publish] final class ReaderFeatures(
  val glossary: Boolean,
  val citation: Boolean,
  val choice: Boolean,
  val transclusion: Boolean,
  val facsimile: Boolean,
  val apparatusAdd: Boolean,
  val apparatusDel: Boolean,
  val apparatusSee: Boolean
):
  def terms: Boolean = glossary || citation
  def inText: Boolean = terms || choice
  def layout: Boolean = transclusion || facsimile
  /** Menu order. Also the data-apparatus-kinds token list. Not data-apparatus. */
  def apparatus: List[String] =
    List(
      Option.when(apparatusAdd)("add"),
      Option.when(apparatusDel)("del"),
      Option.when(apparatusSee)("see")
    ).flatten

private[publish] object ReaderFeatures:
  val empty: ReaderFeatures =
    ReaderFeatures(false, false, false, false, false, false, false, false)

