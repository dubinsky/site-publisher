package org.podval.tools.publish

import org.podval.tools.publish.js.JSLibrary
import org.podval.tools.publish.link.BackLinks
import org.podval.tools.publish.markup.{Markup, XmlLikeMarkup, XmlMarkup}
import org.podval.tools.publish.page.{AssetWithSourcePath, DirectoryPage, EmbeddedAsset, FrontMatter, MarkupPage, Page, PageSource, RealPage, SimpleMarkupPage}
import org.podval.tools.publish.util.{Files, Git, Logging, ObsidianConfig}
import org.podval.xml.{HtmlClass, Xml, XmlElement}
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
  val errors: Errors = Errors(this)

  def error(
    sourcePath: Path,
    kind: PageError.Kind,
    message: String,
    cause: Option[Throwable] = None
  ): Unit = errors.error(PageError(sourcePath, kind, message, cause))

  val ignore: Ignore = Ignore(this)
  val git: Git = Git(sourceDirectory)
  val backLinks: BackLinks = BackLinks()
  val tags: Tags = Tags(this)
  val posts: Posts = Posts(this)

  log.info(s"source directory: $sourceDirectory")
  log.info(s"target directory: $targetDirectory")
  log.info(s"configuration file: $configFile")
  log.debug(s"configuration:\n" + Config.codec.encodeToString(config))
  log.debug(s"ignore rules:\n" + ignore.rules)

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
    val (markup: Option[Markup], parsed: Option[(FrontMatter, Xml.Element)]) =
      sourcePath.fold((None, None)): sourcePath =>
        sourcePath.extension.fold((None, None)): extension =>
          if extension != XmlLikeMarkup.extension
          then
            // Determine markup by the file extension
            val markup: Option[Markup] = Markup.all.find(_.isExtension(extension))
            (markup, None)
          else
            // Parse and disambiguate XML markup by its XML dialect's root elements
            val (frontMatter, xml: Xml.Element) = readAndParse(
              markup = XmlMarkup,
              sourcePath = sourcePath,
              message = "Reading to disambiguate XML dialect",
              firstReading = true,
            )
            val rootElementName: String = xml.getName
            val markup: Option[Markup] = Markup.xmlLike.find(_.xmlDialect.root.contains(rootElementName))
            (markup, Some((frontMatter, xml)))

    def setSource(page: RealPage): Unit = page match
      case markupPage: MarkupPage =>
        for
          sourcePath <- sourcePath
          markup <- markup
        do
          val pageSource: PageSource = PageSource(
            page = markupPage,
            markup = markup,
            sourcePath = sourcePath
          )
          markupPage.setSource(pageSource)
          parsed.foreach((frontMatter, xml) => pageSource.cache(frontMatter, xml))

      case _ => ()

    pages.find(_.path == path.html) match
      case Some(page) => page match
        case markupPage: MarkupPage => setSource(markupPage)
        case _ => ()
        page

      case None =>
        val page: RealPage =
          if path.fileName == DirectoryPage.fileName
          then DirectoryPage(this, path.html)
          else markup match
            case None => AssetWithSourcePath(this, sourcePath.get, path)
            case Some(markup) => SimpleMarkupPage(this, path.html)
        setSource(page)
        addPage(page)
        page

  def readAndParse(
    markup: Markup,
    sourcePath: Path,
    message: String,
    firstReading: Boolean,
  ): (FrontMatter, Xml.Element) =
    log.debug(s"$message: $sourcePath")

    val (frontMatterContent: Option[String], content: String) =
      FrontMatter.split(Files.read(sourcePath.file(sourceDirectory)))

    val frontMatter: FrontMatter = FrontMatter.parse(frontMatterContent) match
      case Right(frontMatter) =>
        frontMatter
      case Left(error) =>
        if firstReading then
          this.error(
            sourcePath = sourcePath,
            kind = PageError.MalformedFrontMatter,
            message = s"Malformed FrontMatter: [$frontMatterContent]",
            cause = Some(error)
          )

        FrontMatter.empty

    val xml: Xml.Element = markup.parse(content) match
      case Right(xml) =>
        xml
      case Left(error) =>
        if firstReading then
          this.error(
            sourcePath = sourcePath,
            kind = PageError.MalformedXml,
            message = s"malformed XML (${markup.extension})",
            cause = Some(error)
          )

        Xml
          .element(XmlElement(markup.xmlDialect.root.head))
          .add(HtmlClass(s"malformed-${markup.extension}"))
          .setText(s"Malformed ${markup.name}: $error")

    (frontMatter, xml)

  // Header pages
  lazy val headerPages: List[HeaderPage] = pages.flatMap(_.headerPage).sortBy(_.priority)

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
    (EmbeddedAsset.embeddedAssets(this) ++ special).foreach(addPage)

    // Scan the directories and add all source pages
    DirectoryPage.scan(this, Seq.empty, sourceDirectory, None)

    // Report conflicting pages
    pages
      .groupBy(_.path)
      .filter(_._2.length > 1)
      .toList
      .foreach((path: Path, pages: List[Page]) => error(
        path,
        PageError.Duplicate,
        s"Duplicates for the path $path: ${pages.map(_.title).tail.mkString(", ")}"
      ))

    // Gather back-links
    pages.foreach(page => backLinks.addBackLinks(page.content.fold(Seq.empty)(_.backLinks)))

    // TODO sort pages topologically based on transclusions

  def generate(): Unit =
    load()

    // Wipe out output directory
    Files.deleteDirectory(targetDirectory)

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
