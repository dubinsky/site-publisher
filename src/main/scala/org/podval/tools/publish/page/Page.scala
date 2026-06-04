package org.podval.tools.publish.page

import org.podval.tools.publish.link.{BackLink, Link}
import org.podval.tools.publish.util.{Date, Files, Icon}
import org.podval.tools.publish.{HeaderPage, PageError, Path, Posts, Site}
import org.podval.xml.Html
import zio.blocks.html.*
import java.io.File
import java.time.{Instant, LocalDate}

abstract class Page(
  val site: Site,
  val path: Path
) derives CanEqual:

  final override def equals(obj: Any): Boolean = obj.asInstanceOf[Matchable] match
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

  final lazy val parent: Option[DirectoryPage] =
    val parentDirectory: Option[Seq[String]] =
      if isDirectory && path.path.length > 1 then Some(path.path.init.init)
      else if !isDirectory && path.path.nonEmpty then Some(path.path.init)
      else None

    parentDirectory
      .map(parentDirectory => site.addPage(None, Path(parentDirectory :+ DirectoryPage.fileName *).html))
      .map {
        case directoryPage: DirectoryPage => directoryPage
        case _ => throw IllegalArgumentException(s"Not a Directory")
      }

  def isAlias: Boolean

  def real: RealPage

  def isDirectory: Boolean

  def source: Option[PageSource]

  def sourcePath: Option[Path]

  private def frontMatter: FrontMatter = source.map(_.cached.frontMatter).getOrElse(FrontMatter.absent)

  // TODO permalink must be absolute
  final def aliases: Seq[Alias] = (
    postPath.toSeq ++
    frontMatter.permalink.toSeq ++
    frontMatter.aliases
  ).map(Alias(site, this, _))

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
  final def dateModifiedGit: Option[Instant] = sourcePath.map(_.toString).flatMap(site.git.modificationDate)

  final def title: String = frontMatter.title.getOrElse(titleDefault)
  def titleDefault: String = titleFromPath
  def titleFromPath: String

  final def description: Option[String] = frontMatter.description.orElse(descriptionDefault)
  protected def descriptionDefault: Option[String] = None

  final def icon: Icon = frontMatter.icon match
    case None => iconDefault
    case Some(icon) => Icon(icon, frontMatter.iconStyle.getOrElse(Icon.Regular))

  protected def iconDefault: Icon
  
  final def lang: String = frontMatter.lang.orElse(langDefault).orElse(site.config.lang).getOrElse("en")
  // TODO set to "en" and clean up overrides
  protected def langDefault: Option[String] = None

  final def backLinks: Seq[BackLink] = source.fold(Seq.empty)(_.backLinks(this))

  final lazy val headerPage: Option[HeaderPage] = Option.when(frontMatter.headerPage)(HeaderPage(
    page = this,
    priority = frontMatter.headerPagePriority.getOrElse(headerPagePriorityDefault)
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
  def pageList(pages: Seq[Page], cls: Option[String] = None): Html.Element = ul(
    className := "page-list",
    pages.map(page => li(page.ref(cls = cls)))
  )
