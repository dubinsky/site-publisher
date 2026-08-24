package org.podval.tools.publish.site

import org.podval.tools.publish.js.JSLibrary
import org.podval.tools.publish.markup.{AsciiDocMarkup, BackLink, Link}
import org.podval.tools.publish.page.{EmbeddedAsset, MarkupPage, PdfPage}
import org.podval.tools.publish.util.{Files, Git, Icon, Logging, Media, ObsidianConfig, SiteOptions}
import org.podval.xml.{Html, Xml}
import zio.blocks.html.*
import com.sun.net.httpserver.{HttpServer, SimpleFileServer}
import com.microsoft.playwright.{Browser, Playwright}
import org.asciidoctor.Asciidoctor
import org.slf4j.{Logger, LoggerFactory}
import java.io.{File, UncheckedIOException}
import java.net.{BindException, InetSocketAddress, URI, URISyntaxException}

final class Site(options: SiteOptions) extends JSLibrary:
  // Site itself is a JavaScript library too
  override def cdn: String = ""
  override def stylesheet: Some[String] = Some(EmbeddedAsset.mainStyleSheet)

  override def headInlineJs: Some[Js] = Some:
    js"""try{if(localStorage.getItem('glossary-expand')==='1')document.documentElement.classList.add('glossary-expand')}catch(e){}"""

  override def inlineJs: Some[Js] = Some(Site.glossaryExpandJs)
  
  // Directories
  val sourceDirectory: File = File(options.sourceDirectoryPath).getAbsoluteFile
  Files.requireExists(sourceDirectory)
  Files.requireDirectory(sourceDirectory)

  def sourceFile(sourcePath: Path): File = sourcePath.file(sourceDirectory)

  val targetDirectory: File = File(sourceDirectory, options.targetDirectoryName)
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

  val config: Config = Config.codec.decode(Files.read(configFile)) match
    case Left(error) => throw IllegalArgumentException("Malformed Config", error)
    case Right(result) => result

  val uri: URI = URI(config.url)

  def isInternalLink(
    href: String,
    errorReporter: PageErrorReporter
  ): Boolean =
    // TODO verify that external link is not broken if the Site is so configured
    try
      val uri: URI = URI(href)
      val isSelf: Boolean = uri.getScheme != null && (/*uri.getHost == null ||*/ uri.getHost == this.uri.getHost)
      if isSelf then errorReporter.error(PageError.SelfLink, href)
      uri.getScheme == null
    catch case e: URISyntaxException => true

  // TODO make HTML converter configurable.
//  private val configurer: Configurer = Configurer.get(options.option("configurer", "Default"))
//  def get(name: String): Configurer = Class
//    .forName(if name.contains(".") then name else s"${Configurer.getClass.getName}$name")
//    .getDeclaredConstructor()
//    .newInstance()
//    .asInstanceOf[Configurer]

  // Components
  val pages: Pages = Pages(this)
  val ignore: Ignore = Ignore(this)
  val git: Git = Git(sourceDirectory)
  val backLinks: BackLinks = BackLinks()
  val tags: Tags = Tags(this)
  val posts: Posts = Posts(this)

  // Errors
  val errors: Errors = Errors(this, treatErrorsAsWarnings = options.treatErrorsAsWarnings)

  def error(
    sourcePath: Path,
    kind: PageError.Kind,
    message: String,
    cause: Option[Throwable] = None
  ): Unit = errors.error(PageError(sourcePath, kind, message, cause))

  // Logging
  Logging.configureLogBack(level = options.logLevel, useLogStash = false)
  val log: Logger = LoggerFactory.getLogger(this.getClass)

  log.info(s"source directory: $sourceDirectory")
  log.info(s"target directory: $targetDirectory")
  log.info(s"configuration file: $configFile")
  log.debug(s"configuration:\n" + Config.codec.encodeToString(config))
  log.debug(s"ignore rules:\n" + ignore.rules)

  // Google Analytics
  val googleAnalytics: Option[String] = if !options.production then None else config.googleAnalytics

  lazy val license: Option[Html.Element] = config.license.map: license =>
    link(rel := "license", titleAttr := license, config.licenseLink.map(licenseLink => href := licenseLink))

  lazy val favicon: Option[Html.Element] =
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

      // Wipe out output directory
      Files.deleteDirectory(targetDirectory)

      // PDFs print via HTTP from already-written HTML/assets; write them last
      // so any assets used are already written.
      val (pdfPages, otherPages) = pages.pages.partition:
        case _: PdfPage => true
        case _ => false
      (otherPages ++ pdfPages).foreach: page =>
        log.debug(s"Writing ${page.path}")
        page.write()

      // Done
      log.info("Done generating!")
    finally
      stopConverters()

  private def load(): Unit =
    // Load all pages
    pages.load()

    // Gather back-links
    for
      page <- pages.pages
      content <- page.content
    do
      backLinks.addBackLinks(
        content.source.markup.xmlDialect.gatherWithParent(
          element = content.xml,
          gatherElement = (element: Xml.Element, parent: Option[Xml.Element]) =>
            if !Link.isInternal(element) then None else BackLink(
              element,
              parent = parent.get,
              from = content.source.page, // TODO go through FullMarkupPages only, use page, remove content.page
              ids = content.ids
            )
        )
      )

    // TODO sort pages topologically based on transclusions

  private var asciidoctorVar: Option[Asciidoctor] = None
  def asciidoctor: Asciidoctor = synchronized:
    asciidoctorVar.getOrElse:
      val result: Asciidoctor = AsciiDocMarkup.asciidoctor(this)
      asciidoctorVar = Some(result)
      result

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
    asciidoctorVar.foreach(_.close())
    browserVar.foreach(_.close())
    playwrightVar.foreach(_.close())

  private var httpServerVar: Option[HttpServer] = None

  private def httpServer: HttpServer = synchronized:
    httpServerVar.getOrElse:
      val result: HttpServer = Site.startHttpServer(targetDirectory)
      val port: Int = result.getAddress.getPort
      if port != Site.defaultHttpPort then
        log.info(s"Port ${Site.defaultHttpPort} in use; HTTP server on $port")
      httpServerVar = Some(result)
      result

  def httpServerPort: Int = httpServer.getAddress.getPort

  private def stopHttpServer(): Unit =
    httpServerVar.foreach(_.stop(0))

  def siteHeader(page: MarkupPage): Html.Element =
    header(className := "site-header",
      div(className := "wrapper",
        a(className := "site-title", href := "/", rel := "author", config.title),
        nav(className := "site-nav",
          input(`type` := "checkbox", id := "nav-trigger"),
          label(`for` := "nav-trigger",
            span(className := "menu-icon",
              //                el("svg", "viewBox" -> "0 0 18 15", "width" -> "18px", "height" -> "15px")(
              //                  el("path", "d" -> "M18,1.484c0,0.82-0.665,1.484-1.484,1.484H1.484C0.665,2.969,0,2.304,0,1.484l0,0C0,0.665,0.665,0,1.484,0 h15.032C17.335,0,18,0.665,18,1.484L18,1.484z M18,7.516C18,8.335,17.335,9,16.516,9H1.484C0.665,9,0,8.335,0,7.516l0,0 c0-0.82,0.665-1.484,1.484-1.484h15.032C17.335,6.031,18,6.696,18,7.516L18,7.516z M18,13.516C18,14.335,17.335,15,16.516,15H1.484 C0.665,15,0,14.335,0,13.516l0,0c0-0.82,0.665-1.483,1.484-1.483h15.032C17.335,12.031,18,12.695,18,13.516L18,13.516z")()
              //                )
            )
          ),
          div(className := "nav-items",
            pages.headerPages.map(_.page.ref()),
            page.up.flatMap(up => Option.when(up.up.isDefined)(up.navRef(Icon.arrowUp))),
            page.prev.map(_.navRef(Icon.arrowLeft)),
            page.next.map(_.navRef(Icon.arrowRight))
          )
        ),
        button(
          `type` := "button",
          id := "glossary-expand-toggle",
          className := "glossary-expand-toggle",
          aria("pressed") := "false",
          titleAttr := "Show glossary definitions in the text",
          Icon.book.html,
          span(className := "glossary-expand-label", "Glossary")
        )
      )
    )
  
  def siteFooter: Html.Element =
    footer(className := "site-footer h-card",
      data(className := "u-url", href := "/"),
      div(className := "wrapper",
        h2(className := "footer-heading", config.title),
        div(className := "footer-col-wrapper",
          div(className := "footer-col footer-col-1",
            ul(className := "contact-list",
              li(className := "p-name", config.author),
              li(a(className := "u-email", href := s"mailto:${config.email}", config.email))
            )
          ),
          div(className := "footer-col footer-col-2",
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
          div(className := "footer-col footer-col-3",
            p(config.description),
            p(Feed.feedFooter)
          )
        )
      )
    )

object Site:
  private lazy val glossaryExpandJs: Js = Js(Files.readResource("/org/podval/tools/publish/site/glossaryExpand.js"))

  val localhost: String = "127.0.0.1"
  val defaultHttpPort: Int = 8000

  // Prefer 8000 so `serve()` has a stable URL. If it is taken (another serve,
  // CI sibling, …), bind an ephemeral port; callers read the real port from
  // `httpServerPort` / `Page.uri`.
  private[site] def startHttpServer(targetDirectory: File): HttpServer =
    val root: java.nio.file.Path = targetDirectory.getAbsoluteFile.toPath
    try
      startHttpServerOn(root, defaultHttpPort)
    catch
      case e: UncheckedIOException if e.getCause.isInstanceOf[BindException] =>
        startHttpServerOn(root, 0)

  private def startHttpServerOn(root: java.nio.file.Path, port: Int): HttpServer =
    val result: HttpServer = SimpleFileServer.createFileServer(
      InetSocketAddress(localhost, port),
      root,
      SimpleFileServer.OutputLevel.NONE
    )
    result.start()
    result

  def main(args: Array[String]): Unit = Site(SiteOptions.forArgs(args)).generate()

  // TODO make chunked HTML the front page of chumashquestions:
  // allow the chunked index be called "index.html";
  // allow permalink to override slf-added index file...
  @main def generate(): Unit = Site(SiteOptions(
    sourceDirectoryPath =
      "/home/dub/OpenTorah/chumashquestions.org/",
//      "/home/dub/OpenTorah/opentorah.org/docs",
    //  "/home/dub/OpenTorah/alter-rebbe.org"
//      "/home/dub/Podval/dub.podval.org",
    //  "/home/dub/Podval/www.podval.org"
    logLevelOpt = Some("INFO"),
    treatErrorsAsWarnings = true
  ))
    .serve()

