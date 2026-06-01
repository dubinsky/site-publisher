package org.podval.tools.publish

import org.podval.tools.publish.js.JSLibrary
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
  override def stylesheet: Some[String] = Some(Asset.mainStyleSheet)

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
  val errors: Errors = Errors(this)
  val ignore: Ignore = Ignore(this)
  val git: Git = Git(sourceDirectory)
  val backLinks: BackLinks = BackLinks()
  val tags: Tags = Tags(this)
  val posts: Posts = Posts(this)

  log.info(s"source directory: $sourceDirectory")
  log.info(s"target directory: $targetDirectory")
  log.info(s"configuration file: $configFile")
  log.debug(s"configuration:\n" + Config.codec.encodeToString(config))
  log.info(s"ignore rules:\n" + ignore.rules)

  def postsDirectoryName: String = "_posts"
  def draftsDirectoryName: Option[String] = Option.when(includeDrafts)("_drafts")
  def dailyNotesDirectoryName: Option[String] = obsidianConfig.daysFolder
  
  private var pagesVar: List[Page] = List.empty
  def pages: List[Page] = pagesVar

  def addPage(page: Page): Unit =
    pagesVar = pagesVar.appended(page)
    // Add implied directories
    page.parent
    // Add alias pages
    page.aliases.foreach(addPage)

  def addPage(
    sourcePath: Option[Path],
    path: Path
  ): Page =
    def setSource(page: Page.Real): Unit = page match
      case markupPage: MarkupPage =>
        for
          sourcePath <- sourcePath
          markup <- Markup.of(sourcePath)
        do
          markupPage.setSource(markup, sourcePath)
      case _ => ()

    pages.find(_.path == path.html) match
      case Some(page) => page match
        case markupPage: MarkupPage => setSource(markupPage)
        case _ => ()
        page

      case None =>
        val page: Page.Real =
          if path.fileName == Directory.fileName
          then Directory(this, path.html)
          else sourcePath.flatMap(Markup.of) match
            case None => Asset.AssetWithSource(this, sourcePath.get, path)
            case Some(markup) => MarkupPage.Simple(this, path.html)
        setSource(page)
        addPage(page)
        page

  // Header pages
  lazy val headerPages: List[HeaderPage] = pages.flatMap(_.headerPage).sortBy(_.priority)

  // Social links
  val socialLinks: Seq[SocialLink] = Seq(
    config.social.github.map(SocialLink.GitHub(_)),
    config.social.twitter.map(SocialLink.Twitter(_)),
    config.social.linkedin.map(SocialLink.LinkedIn(_))
  ).flatten

  private def addAllPages(): Unit =
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

    (Asset.embeddedAssets(this) ++ special).foreach(addPage)

    // Scan the directories and add all source pages
    Directory.scan(this, Seq.empty, sourceDirectory, None)

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
    pages.foreach(page => backLinks.addBackLinks(page.backLinks))

    // TODO sort pages topologically based on transclusions

    // Write pages
    pages.foreach(_.write())

    // Done
    log.info("Done!")

object Site:
  def main(args: Array[String]): Unit = Cli.main(Array(
    "--log-level=INFO",
    "--treat-errors-as-warnings=true",
    "/home/dub/Podval/dub.podval.org"
  ))
