package org.podval.tools.publish.page

import org.podval.tei.EntityKind
import org.podval.tools.publish.link.Link
import org.podval.tools.publish.util.{Date, Icon}
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

  def up: Option[Page] = parent

  final lazy val parent: Option[DirectoryPage] =
    val parentDirectory: Option[Seq[String]] =
      if isDirectory && path.path.length > 1 then Some(path.path.init.init)
      else if !isDirectory && path.path.nonEmpty then Some(path.path.init)
      else None

    parentDirectory
      .filterNot(_.isEmpty)
      .map(parentDirectory => site.pages.getOrAddDirectory(Path(parentDirectory :+ DirectoryPage.fileName *).html))

  def isAlias: Boolean

  def real: RealPage

  def isDirectory: Boolean

  def source: Option[PageSource]

  // TODO not final: overridden in AssetWithSourcePath
  def sourcePath: Option[Path] = source.map(_.sourcePath)

  final def content: Option[PageContent] = source.map(_.content)
  final def content[A](f: PageContent => Option[A]): Option[A] = content.flatMap(f)
  
  private def frontMatter: FrontMatter = content.fold(FrontMatter.absent)(_.frontMatter)

  // TODO permalink must be absolute
  final def aliases: Seq[Alias] = (postPath.toSeq ++ frontMatter.permalink.toSeq ++ frontMatter.aliases)
    .map(Alias(site, this, _))

  private def postPath: Option[String] = if !frontMatter.post then None else date match
    case None =>
      site.error(path, PageError.NoDate, s"No date for an automatic blog post")
      None
    case Some(date) =>
      val title: String = frontMatter.postTitle.getOrElse(path.fileName) // TODO titleFromPath?
      Some(Posts.path(date.localDate, title).html.withoutHtml.toString)

  final def tags: List[String] = frontMatter.tags
  final def math: Boolean = site.config.math || frontMatter.math

  final lazy val postDate: Option[LocalDate] = Posts.date(path)
  final def isPost: Boolean = postDate.isDefined || frontMatter.post // TODO take permalink into account?
  final def date: Option[Date] = postDate.map(Date.Local(_)).orElse(content(_.frontMatter.date))
  final def dateModified: Option[Date] = content(_.frontMatter.modifiedTime)
  final def dateModifiedGit: Option[Instant] = sourcePath.map(_.toString).flatMap(site.git.modificationDate)

  final lazy val headerPage: Option[HeaderPage] = Option.when(frontMatter.headerPage)(HeaderPage(
    page = this,
    priority = frontMatter.headerPagePriority.getOrElse(headerPagePriorityDefault)
  ))

  protected def headerPagePriorityDefault: Int = 0
  
  final def author: Option[String] = content(_.frontMatter.author)

  // TODO take content into account
  final def title: String = content(_.frontMatter.title).getOrElse(titleDefault)
  def titleDefault: String = titleFromPath
  def titleFromPath: String

  final def description: Option[String] = content(_.frontMatter.description).orElse(descriptionDefault)
  protected def descriptionDefault: Option[String] = None

  final def icon: Icon = frontMatterIcon.getOrElse(iconDefault)
  private def frontMatterIcon: Option[Icon] = frontMatter
    .icon
    .map(icon => Icon(icon, frontMatter.iconStyle.getOrElse(Icon.Regular)))

  protected def iconDefault: Icon
  
  final def paginate: Boolean = frontMatter.paginate
  
  final def lang: String = content(_.frontMatter.lang).orElse(langDefault).orElse(site.config.lang).getOrElse("en")
  // TODO set to "en" and clean up overrides
  protected def langDefault: Option[String] = None
  
  final def entityKind: Option[EntityKind] = content(_.entityKind)
  
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

  final def navRef(icon: Icon): Html.Element = ref(
    cls = Some("nav-item"),
    icon = Some(icon),
    withTitle = false
  )

object Page:
  def pageList(pages: Seq[Page], cls: Option[String] = None): Html.Element = ul(
    className := "page-list",
    pages.map(page => li(page.ref(cls = cls)))
  )
