package org.podval.tools.publish

import org.podval.tools.publish.util.{Files, Logging}
import org.slf4j.{Logger, LoggerFactory}
import org.slf4j.event.Level
import java.io.File

final class Site(
  sourceDirectoryPath: String,
  production: Boolean,
  targetDirectoryName: String,
  includeDrafts: Boolean,
  val treatErrorsAsWarnings: Boolean,
  logLevel: Level = Level.INFO
):
  private given CanEqual[File, File] = CanEqual.derived

  Logging.configureLogBack(level = logLevel, useLogStash = false)
  val log: Logger = LoggerFactory.getLogger(this.getClass)

  private val config: Config = Config(
    sourceDirectoryPath,
    targetDirectoryName,
    includeDrafts
  )

  def sourceDirectory: File = config.sourceDirectory
  def targetDirectory: File = config.targetDirectory

  def title: String = config.title
  def description: String = config.description
  def url: String = config.url
  def author: String = config.author
  def email: String = config.email
  def lang: String = config.lang.getOrElse("en")
  def googleAnalytics: Option[String] = Option.when(production)(config.googleAnalytics).flatten
  val socialLinks: Seq[SocialLink] = config.socialLinks

  private var pagesVar: List[Page] = List.empty
  def pages: List[Page] = pagesVar

  def addPage[P <: Page](page: P): P =
    pagesVar = pagesVar.appended(page)
    page

  // Add automatic pages
  val errors: Errors = addPage(Errors(this))
  val tags: Tags = addPage(Tags(this))
  val posts: Posts = addPage(Posts(
    this,
    postsDirectoryName = config.postsDirectoryName,
    draftsDirectoryName = config.draftsDirectoryName,
    dailyNotesDirectoryName = config.dailyNotesDirectoryName
  ))

  // TODO make a lazy val
  def markupPages: List[MarkupPage] = pages.collect { case page: MarkupPage => page }
  def find(path: Path): Option[MarkupPage] = markupPages.find(_.path == path)

  // Back-links
  private val backLinks: BackLinks = BackLinks()
  def backLinks(page: Page): Seq[(MarkupPage, List[BackLinks.BackLink])] = backLinks.backLinks(page)

  // Header pages
  lazy val headerPages: List[HeaderPage] = markupPages.flatMap(_.headerPage).sortBy(_.priority)

  def generate(): Unit =
    // Wipe out output directory
    Files.deleteDirectory(targetDirectory)

    // Write embedded resources
    Asset.embeddedAssets(this).foreach(_.write())

    // Scan the directories and add all source pages
    scanDirectory(Seq.empty, sourceDirectory)

    // Report conflicting pages
    pages
      .groupBy(_.path)
      .filter(_._2.length > 1)
      .toList
      .foreach((path: Path, pages: List[Page]) => errors.error(PageError(
        PageError.Duplicate, path, s"Duplicates for the path $path: ${pages.map(_.title).tail.mkString(", ")}"
      )))

    // Add directory pages
    Directory.addParentDirectories(this)

    // Gather back-links
    markupPages.foreach(page => backLinks.addBackLinks(page.backLinks))

    // TODO sort pages topologically based on transclusions

    // Write pages
    pages.foreach(_.write())

    // Write synthetic assets
    Sitemap(this).write()
    Robots(this).write()
    Feed(this).write()

    // Done
    log.info("Done!")

  private def scanDirectory(directoryPath: Seq[String], directory: File): Unit =
    Files.requireExists(directory)
    Files.requireDirectory(directory)

    val (files: List[File], directories: List[File]) = Files
      .list(directory)
      .filterNot(config.exclude)
      .partition(_.isFile)

    files.foreach(scanFile(directoryPath, _))

    directories.foreach(directory => scanDirectory(directoryPath :+ directory.getName, directory))

  private def scanFile(directoryPath: Seq[String], file: File): Unit =
    Files.requireExists(file)
    Files.requireFile(file)
    val (name: String, extension: Option[String]) = Files.nameAndExtension(file.getName)
    val sourcePath: Path = Path(directoryPath :+ name, extension)
    extension.flatMap(extension => Markup.all.find(_.isExtension(extension))) match
      case None =>
        addPage(Asset.AssetWithSource(this, sourcePath))

      case Some(markup) =>
        val page: MarkupPage = find(sourcePath.html).getOrElse:
          val path: Path = posts.path(sourcePath).getOrElse(sourcePath).html
          addPage(
            if path.fileName == Directory.fileName
            then Directory(this, path)
            else MarkupPage.Simple(this, path)
          )

        page.withSource(markup, sourcePath)

object Site:
  def main(args: Array[String]): Unit = Cli.main(Array(
    "--log-level=INFO",
//    "--treat-errors-as-warnings=true",
    "/home/dub/Podval/dub.podval.org"
  ))
