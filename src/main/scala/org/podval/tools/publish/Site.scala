package org.podval.tools.publish

import org.podval.tools.publish.js.JSLibrary
import org.podval.tools.publish.util.{Files, Logging, ObsidianConfig}
import org.slf4j.{Logger, LoggerFactory}
import org.slf4j.event.Level
import java.io.File

final class Site(
  val sourceDirectory: File,
  val targetDirectory: File,
  production: Boolean,
  includeDrafts: Boolean,
  val treatErrorsAsWarnings: Boolean,
  config: Config,
  obsidianConfig: ObsidianConfig
) extends JSLibrary:
  // Log some
  val log: Logger = LoggerFactory.getLogger(this.getClass)
  log.info(s"source directory: $sourceDirectory")
  log.debug(s"Config:\n" + Config.codec.encodeToString(config))

  // Site itself is a JavaScript library too
  override def cdn: String = ""
  override def stylesheet: Some[String] = Some(Asset.mainStyleSheet)
  
  def title: String = config.title
  def description: String = config.description
  def url: String = config.url
  def author: String = config.author
  def email: String = config.email
  def math: Boolean = config.math
  def lang: String = config.lang.getOrElse("en")
  def googleAnalytics: Option[String] = if production then config.googleAnalytics else None

  val socialLinks: Seq[SocialLink] = Seq(
    config.social.github.map(SocialLink.GitHub(_)),
    config.social.twitter.map(SocialLink.Twitter(_)),
    config.social.linkedin.map(SocialLink.LinkedIn(_))
  ).flatten

  private def postsDirectoryName: String = "_posts"
  private def draftsDirectoryName: Option[String] = Option.when(includeDrafts)("_drafts")
  private def dailyNotesDirectoryName: Option[String] = obsidianConfig.daysFolder

  def exclude: Set[String] = config.exclude
  
  val include: Set[String] =
    Set(postsDirectoryName) ++
    draftsDirectoryName.toSet ++
    dailyNotesDirectoryName.toSet
  
  // Components
  val backLinks: BackLinks = BackLinks()
  val errors: Errors = Errors(this)
  val tags: Tags = Tags(this)
  val posts: Posts = Posts(
    this,
    postsDirectoryName = postsDirectoryName,
    draftsDirectoryName = draftsDirectoryName,
    dailyNotesDirectoryName = dailyNotesDirectoryName
  )

  private var pagesVar: List[Page] = List.empty
  def pages: List[Page] = pagesVar

  def addPage(page: Page): Unit =
    pagesVar = pagesVar.appended(page)
    // Add implied directories
    page.parent
    // Add alias pages
    page.aliases.foreach(addPage)

  // TODO make a lazy val
  def markupPages: List[MarkupPage] = pages.collect { case page: MarkupPage => page }

  def find(path: Path): Option[MarkupPage] = markupPages.find(_.path == path)

  // Header pages
  lazy val headerPages: List[HeaderPage] = markupPages.flatMap(_.headerPage).sortBy(_.priority)

  private def addAllPages(): Unit =
    // Add embedded resources
    Asset.embeddedAssets(this).foreach(addPage)

    // Add synthetic assets
    addPage(Sitemap(this))
    addPage(Robots(this))
    addPage(Feed(this))

    // Add automatic pages
    addPage(errors)
    addPage(tags)
    addPage(posts)

    // Scan the directories and add all source pages
    Directory.scan(this, Seq.empty, sourceDirectory)

  def generate(): Unit =
    // Wipe out output directory
    Files.deleteDirectory(targetDirectory)

    // Add all pages
    addAllPages()

    // Report conflicting pages
    pages
      .groupBy(_.path)
      .filter(_._2.length > 1)
      .toList
      .foreach((path: Path, pages: List[Page]) => errors.error(PageError(
        PageError.Duplicate, path, s"Duplicates for the path $path: ${pages.map(_.title).tail.mkString(", ")}"
      )))
    
    // Gather back-links
    markupPages.foreach(page => backLinks.addBackLinks(page.backLinks))

    // TODO sort pages topologically based on transclusions

    // Write pages
    pages.foreach(_.write())

    // Done
    log.info("Done!")

object Site:
  private val configFileName: String = "_site_config.yml"

  def apply(
    sourceDirectoryPath: String,
    production: Boolean,
    targetDirectoryName: String,
    includeDrafts: Boolean,
    treatErrorsAsWarnings: Boolean,
    logLevel: Level = Level.INFO
  ): Site =
    Logging.configureLogBack(level = logLevel, useLogStash = false)

    val sourceDirectory: File = File(sourceDirectoryPath).getAbsoluteFile
    Files.requireExists(sourceDirectory)
    Files.requireDirectory(sourceDirectory)

    val targetDirectory: File = File(sourceDirectory, targetDirectoryName)
    targetDirectory.mkdirs()
    Files.requireExists(targetDirectory)
    Files.requireDirectory(targetDirectory)

    val configFile: File = File(sourceDirectory, configFileName)
    Files.requireExists(configFile)
    Files.requireFile(configFile)

    val config: Config = Config.codec.decode(Files.read(configFile)) match
      case Left(error) => throw IllegalArgumentException("Malformed Config", error)
      case Right(result) => result

    val obsidianConfig: ObsidianConfig = ObsidianConfig(sourceDirectory)

    new Site(
      sourceDirectory = sourceDirectory,
      targetDirectory = targetDirectory,
      production = production,
      includeDrafts = includeDrafts,
      treatErrorsAsWarnings = treatErrorsAsWarnings,
      config = config,
      obsidianConfig = obsidianConfig
    )
  
  def main(args: Array[String]): Unit = Cli.main(Array(
    "--log-level=INFO",
    "--treat-errors-as-warnings=true",
    "/home/dub/Podval/dub.podval.org"
  ))
