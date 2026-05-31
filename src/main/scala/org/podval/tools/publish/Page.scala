package org.podval.tools.publish

import org.podval.tools.publish.util.{Date, Files, Icon}
import org.podval.xml.Html
import zio.blocks.html.*
import java.io.File
import java.time.LocalDate

abstract class Page(
  val site: Site,
  val path: Path
) derives CanEqual:

  final override def equals(obj: Any): Boolean = obj match
    case that: Page => this.path == that.path
    case _ => false

  final override def hashCode(): Int = path.hashCode()

  final override def toString: String =
    val source: String = sourcePath match
      case Some(sourcePath) if sourcePath.path != path.path  => s" ($sourcePath)"
      case _ => ""

    s"${getClass.getSimpleName} $path$source"

  def write(): Unit

  final protected def targetFile: File = path.file(site.targetDirectory)

  final lazy val parent: Option[Directory] =
    val parentDirectory: Option[Seq[String]] =
      if isDirectory && path.path.length > 1 then Some(path.path.init.init)
      else if !isDirectory && path.path.nonEmpty then Some(path.path.init)
      else None

    parentDirectory
      .map(parentDirectory => site.addPage(None, Path(parentDirectory :+ Directory.fileName *).html))
      .map {
        case directory: Directory => directory
        case _ => throw IllegalArgumentException(s"Not a Directory")
      }

  def isAlias: Boolean

  def real: Page.Real

  def isDirectory: Boolean

  def source: Option[MarkupPage.Source]

  def sourcePath: Option[Path]

  private def frontMatter: FrontMatter = source.map(_.cached.frontMatter).getOrElse(FrontMatter.absent)

  // TODO permalink must be absolute
  final def aliases: Seq[Page.Alias] = (
    postPath.toSeq ++
    frontMatter.permalink.toSeq ++
    frontMatter.aliases
  ).map(Page.Alias(site, this, _))

  private def postPath: Option[String] = if !frontMatter.post then None else date match
    case None =>
      site.errors.error(PageError(PageError.NoDate, path, s"No date for an automatic blog post"))
      None
    case Some(date) =>
      val title: String = frontMatter.postTitle.getOrElse(path.fileName) // TODO titleFromPath?
      Some(Posts.path(date.localDate, title).html.withoutHtml.toString)

  final def tags: List[String] = frontMatter.tags
  final def author: Option[String] = frontMatter.author
  final def math: Boolean = site.config.math || frontMatter.math

  final lazy val postDate: Option[LocalDate] = Posts.date(path)
  final def isPost: Boolean = postDate.isDefined || frontMatter.post // TODO take permalink into account?
  final def date: Option[Date] = postDate.map(Date.Local(_)).orElse(frontMatter.date)
  final def dateModified: Option[Date] = frontMatter.modifiedTime

  final def title: String = frontMatter.title.getOrElse(titleDefault)
  def titleDefault: String = titleFromPath
  def titleFromPath: String

  final def description: Option[String] = frontMatter.description.orElse(descriptionDefault)
  protected def descriptionDefault: Option[String] = None

  final def icon: Icon = frontMatter.icon.getOrElse(iconDefault)
  protected def iconDefault: Icon
  
  final def lang: String = frontMatter.lang.orElse(langDefault).orElse(site.config.lang).getOrElse("en")
  // TODO set to "en" and clean up overrides
  protected def langDefault: Option[String] = None

  def backLinks: Seq[BackLinks.BackLink]
  
  final lazy val headerPage: Option[HeaderPage] = frontMatter
    .headerPage
    .filter(_.include)
    .map(headerPage => HeaderPage(
      page = this,
      priority = headerPage.priority.getOrElse(headerPagePriorityDefault)
    ))

  protected def headerPagePriorityDefault: Int = 0

  final def ref(
    cls: Option[String] = None,
    withTitle: Boolean = true,
    withIcon: Boolean = true,
    icon: Option[Icon] = None
  ): Html.Element =
    val clss = (Seq("page-ref") ++ cls.toSeq).mkString(" ")
    val pageLink: Link = Link(this, fragment = None, intrapage = false)
    a(
      className := clss,
      // TODO this results in duplicate class attribute!!!
//      className := "page-ref",
//      cls.map(cls => className += cls),
      href := pageLink.url,
      Option.when(withIcon)(icon.getOrElse(this.icon).html),
      Option.when(withTitle)(pageLink.titleReal)
    )

object Page:
  trait WithContent extends Page:
    final override def write(): Unit = Files.write(targetFile, content)
    def content: String

  trait NonDirectory extends Page:
    final override def isDirectory: Boolean = false
    final override def titleFromPath: String = path.fileName

  abstract class Real(
    site: Site,
    path: Path
  ) extends Page(
    site,
    path
  ):
    final override def isAlias: Boolean = false
    final override def real: Real = this

  final class Alias(
    site: Site,
    val page: Page,
    val alias: String
  ) extends Page(
    site,
    path = page.path.relativize(alias).html
  ) with NonDirectory with WithContent:
    override def isAlias: Boolean = true
    override def real: Real = page.real
    override def source: Option[MarkupPage.Source] = None
    override def titleDefault: String = path.fileName
    override protected def iconDefault: Icon = Icon("link", Icon.Solid)
    override def sourcePath: Option[Path] = None
    override def backLinks: Seq[BackLinks.BackLink] = Seq.empty
    override def content: String = s"""<head><meta http-equiv="Refresh" content="0; URL=${page.real.path}"/></head>"""

  def pageList(pages: Seq[Page], cls: Option[String] = None): Html.Element = ul(
    className := "page-list",
    pages.map(page => li(page.ref(cls = cls)))
  )
