package org.podval.tools.publish

import org.podval.tools.publish.js.JSLibrary
import org.podval.tools.publish.link.BackLinks
import org.podval.tools.publish.markup.Markups
import org.podval.tools.publish.page.EmbeddedAsset
import org.podval.tools.publish.util.{Files, Git, Icon, Logging, ObsidianConfig}
import org.podval.xml.Html
import zio.blocks.html.*
import org.slf4j.{Logger, LoggerFactory}
import org.slf4j.event.Level
import java.io.File

final class Site(
  sourceDirectoryPath: String,
  val production: Boolean,
  targetDirectoryName: String,
  includeDrafts: Boolean,
  val treatErrorsAsWarnings: Boolean,
  logLevel: Level = Level.INFO
) extends JSLibrary:
  // Site itself is a JavaScript library too
  override def cdn: String = ""
  override def stylesheet: Some[String] = Some(EmbeddedAsset.mainStyleSheet)

  // Logging
  Logging.configureLogBack(level = logLevel, useLogStash = false)
  val log: Logger = LoggerFactory.getLogger(this.getClass)

  // Directories
  val sourceDirectory: File = File(sourceDirectoryPath).getAbsoluteFile
  Files.requireExists(sourceDirectory)
  Files.requireDirectory(sourceDirectory)

  val targetDirectory: File = File(sourceDirectory, targetDirectoryName)
  targetDirectory.mkdirs()
  Files.requireExists(targetDirectory)
  Files.requireDirectory(targetDirectory)

  // Configuration
  private val configFile: File = File(sourceDirectory, "_site_config.yml")
  Files.requireExists(configFile)
  Files.requireFile(configFile)

  val config: Config = Config.codec.decode(Files.read(configFile)) match
    case Left(error) => throw IllegalArgumentException("Malformed Config", error)
    case Right(result) => result

  private val obsidianConfig: ObsidianConfig = ObsidianConfig(sourceDirectory)

  // Components
  val markups: Markups = Markups.default
  val pages: Pages = Pages(this)
  val errors: Errors = Errors(this)
  val ignore: Ignore = Ignore(this)
  val git: Git = Git(sourceDirectory)
  val backLinks: BackLinks = BackLinks()
  val tags: Tags = Tags(this)
  val posts: Posts = Posts(this)

  def error(
    sourcePath: Path,
    kind: PageError.Kind,
    message: String,
    cause: Option[Throwable] = None
  ): Unit = errors.error(PageError(sourcePath, kind, message, cause))

  log.info(s"source directory: $sourceDirectory")
  log.info(s"target directory: $targetDirectory")
  log.info(s"configuration file: $configFile")
  log.debug(s"configuration:\n" + Config.codec.encodeToString(config))
  log.debug(s"ignore rules:\n" + ignore.rules)

  def postsDirectoryName: String = "_posts"
  def draftsDirectoryName: Option[String] = Option.when(includeDrafts)("_drafts")
  def dailyNotesDirectoryName: Option[String] = obsidianConfig.daysFolder

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

  def generate(): Unit =
    load()

    // Wipe out output directory
    Files.deleteDirectory(targetDirectory)

    // Write pages
    pages.pages.foreach(_.write())

    // Done
    log.info("Done!")

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
                li(
                  a(
                    rel := "me",
                    href := social.href,
                    target := "_blank",
                    titleAttr := social.title,
                    Icon.brand(social.icon).html,
                    span(className := "username", social.userName)
                  )
                )
              ))
            )
          ),
          div(className := "footer-col footer-col-3",
            p(config.description),
            p(
              // TODO move to Feed
              a(
                href := Feed.path.toString,
                Icon.rss.html,
                span(className := "rss-feed", "RSS feed")
              )
            )
          )
        )
      )
    )

object Site:
  def main(args: Array[String]): Unit = Cli.main(Array(
    "--log-level=INFO",
    "--treat-errors-as-warnings=true",
//    "/home/dub/OpenTorah/alter-rebbe.org"
  "/home/dub/Podval/dub.podval.org"
//    "/home/dub/Podval/www.podval.org"
  ))
