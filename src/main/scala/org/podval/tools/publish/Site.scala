package org.podval.tools.publish

import org.podval.tools.publish.js.JSLibrary
import org.podval.tools.publish.link.BackLinks
import org.podval.tools.publish.page.{DirectoryPage, EmbeddedAsset, Page}
import org.podval.tools.publish.util.{Files, Git, Logging, ObsidianConfig}
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
  val pages: Pages = Pages(this)
  val ignore: Ignore = Ignore(this)
  val git: Git = Git(sourceDirectory)
  val backLinks: BackLinks = BackLinks()
  val tags: Tags = Tags(this)
  val posts: Posts = Posts(this)

  // Errors
  val errors: Errors = Errors(this)

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

  // Header pages
  lazy val headerPages: List[HeaderPage] = pages.pages.flatMap(_.headerPage).sortBy(_.priority)

  // Social links
  val socialLinks: Seq[SocialLink] = Seq(
    config.social.github.map(SocialLink.GitHub(_)),
    config.social.twitter.map(SocialLink.Twitter(_)),
    config.social.linkedin.map(SocialLink.LinkedIn(_))
  ).flatten

  private def load(): Unit =
    val special: Seq[Page] = Seq(
      // Synthetic assets
      Sitemap(this),
      Robots(this),
      Feed(this),

      // Automatic pages
      errors,
      tags,
      posts
    )

    // Add special pages
    (EmbeddedAsset.embeddedAssets(this) ++ special).foreach(pages.add)

    // Scan the directories and add all source pages
    pages.scan(Seq.empty, sourceDirectory, None)

    // Report conflicting pages
    pages
      .pages
      .groupBy(_.path)
      .filter(_._2.length > 1)
      .toList
      .foreach((path: Path, pages: List[Page]) => error(
        path,
        PageError.Duplicate,
        s"Duplicates for the path $path: ${pages.map(_.title).tail.mkString(", ")}"
      ))

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

object Site:
  def main(args: Array[String]): Unit = Cli.main(Array(
    "--log-level=INFO",
    "--treat-errors-as-warnings=true",
    "/home/dub/Podval/dub.podval.org"
  ))
