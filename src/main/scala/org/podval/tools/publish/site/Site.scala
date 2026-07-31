package org.podval.tools.publish.site

import org.podval.tools.publish.js.JSLibrary
import org.podval.tools.publish.link.BackLinks
import org.podval.tools.publish.page.{EmbeddedAsset, MarkupPage}
import org.podval.tools.publish.util.{Files, Git, Icon, Logging, ObsidianConfig, Options}
import org.podval.xml.Html
import org.slf4j.{Logger, LoggerFactory}
import zio.blocks.html.*
import java.io.File
import java.net.URI

final class Site(options: Options) extends JSLibrary:
  // Site itself is a JavaScript library too
  override def cdn: String = ""
  override def stylesheet: Some[String] = Some(EmbeddedAsset.mainStyleSheet)
  
  // Directories
  val sourceDirectory: File = File(options.positional(0)).getAbsoluteFile
  Files.requireExists(sourceDirectory)
  Files.requireDirectory(sourceDirectory)

  def sourceFile(sourcePath: Path): File = sourcePath.file(sourceDirectory)

  val targetDirectory: File = File(sourceDirectory, options.option("target-directory-name", "_site"))
  targetDirectory.mkdirs()
  Files.requireExists(targetDirectory)
  Files.requireDirectory(targetDirectory)

  // Posts and daily notes directories
  private val obsidianConfig: ObsidianConfig = ObsidianConfig(sourceDirectory)
  def postsDirectoryName: String = "_posts"
  val draftsDirectoryName: Option[String] = Option.when(options.booleanOption("include-drafts"))("_drafts")
  def dailyNotesDirectoryName: Option[String] = obsidianConfig.daysFolder

  // Configuration
  private val configFile: File = File(sourceDirectory, "_site_config.yml")
  Files.requireExists(configFile)
  Files.requireFile(configFile)

  val config: Config = Config.codec.decode(Files.read(configFile)) match
    case Left(error) => throw IllegalArgumentException("Malformed Config", error)
    case Right(result) => result

  val uri: URI = URI(config.url)

  def isSelf(uri: URI): Boolean =
    uri.getScheme != null && (/*uri.getHost == null ||*/ uri.getHost == this.uri.getHost)

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
  val errors: Errors = Errors(this, treatErrorsAsWarnings = options.booleanOption("treat-errors-as-warnings"))

  def error(
    sourcePath: Path,
    kind: PageError.Kind,
    message: String,
    cause: Option[Throwable] = None
  ): Unit = errors.error(PageError(sourcePath, kind, message, cause))

  // Logging
  Logging.configureLogBack(level = options.option("log-level", "DEBUG"), useLogStash = false)
  val log: Logger = LoggerFactory.getLogger(this.getClass)

  log.info(s"source directory: $sourceDirectory")
  log.info(s"target directory: $targetDirectory")
  log.info(s"configuration file: $configFile")
  log.debug(s"configuration:\n" + Config.codec.encodeToString(config))
  log.debug(s"ignore rules:\n" + ignore.rules)

  // Google Analytics
  val googleAnalytics: Option[String] = if !options.booleanOption("production") then None else config.googleAnalytics

  // Social links
  private val socialLinks: Seq[SocialLink] = Seq(
    config.social.github.map(SocialLink.GitHub(_)),
    config.social.twitter.map(SocialLink.Twitter(_)),
    config.social.linkedin.map(SocialLink.LinkedIn(_))
  ).flatten

  private def load(): Unit =
    // Load all pages
    pages.load()

    // Gather back-links
    pages.pages.foreach(page => backLinks.addBackLinks(page.content.fold(Seq.empty)(_.backLinks)))

    // TODO sort pages topologically based on transclusions

  // TODO from Grok:
  //- Description: `generate()` deletes the entire target directory, then writes page-by-page. A crash mid-write leaves a partial site; concurrent readers (local server, CI publish) can observe a wiped tree. No temp-dir + atomic rename.
  //- Suggestion: Write to a staging directory (or `_site.tmp`) and atomically replace `_site`. Optionally preserve mtimes for unchanged assets to speed deploys.
  def generate(): Unit =
    load()

    // Wipe out output directory
    Files.deleteDirectory(targetDirectory)

    // Write pages
    pages.pages.foreach: page =>
      log.debug(s"Writing ${page.path}")
      page.write()

    // Done
    log.info("Done!")

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
  def main(args: Array[String]): Unit =
    Site(Options(args, environmentVariablesPrefix = "SITE_PUBLISHER"))
      .generate()

  // TODO same "unresolved reference" error is now logged multiple times - chunking?
  @main def generate(): Unit = main(Array(
    "--log-level=INFO",
    "--treat-errors-as-warnings=true",
    "/home/dub/OpenTorah/opentorah.org/docs"
//  "/home/dub/OpenTorah/alter-rebbe.org"
//  "/home/dub/Podval/dub.podval.org"
//  "/home/dub/Podval/www.podval.org"
  ))
