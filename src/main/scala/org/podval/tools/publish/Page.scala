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

  final lazy val parent: Option[Directory] = Directory.parent(site, this)
  
  def isDirectory: Boolean

  def source: Option[MarkupPage.Source]

  def sourcePath: Option[Path]

  private def frontMatter: FrontMatter =
    source.map(_.cached.frontMatter).getOrElse(FrontMatter.absent)

  final def aliases: List[String] = frontMatter.aliases
  final def tags: List[String] = frontMatter.tags
  final def author: String = frontMatter.author.getOrElse(site.author)
  final def math: Boolean = frontMatter.math

  final protected lazy val postDate: Option[LocalDate] = Posts.date(path)
  final def isPost: Boolean = postDate.isDefined
  final def date: Option[Date] = postDate.map(Date.Local(_)).orElse(frontMatter.date)
  final def dateModified: Option[Date] = frontMatter.modified_time

  final def title: String = frontMatter.title.getOrElse(titleDefault)
  protected def titleDefault: String

  final def description: Option[String] = frontMatter.description.orElse(descriptionDefault)
  protected def descriptionDefault: Option[String] = None

  final def icon: Icon = frontMatter.icon.getOrElse(iconDefault)
  protected def iconDefault: Icon
  
  final def lang: String = frontMatter.lang.orElse(langDefault).getOrElse(site.lang)
  protected def langDefault: Option[String] = None
  
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
      Option.when(withIcon)(icon.getOrElse(this.icon).htmlSpan),
      Option.when(withTitle)(pageLink.title)
    )

object Page:
  trait WithContent extends Page:
    final override def write(): Unit = Files.write(targetFile, content)
    def content: String

  def pageList(pages: Seq[Page], cls: Option[String] = None): Html.Element = ul(
    className := "page-list",
    pages.map(page => li(page.ref(cls = cls)))
  )
