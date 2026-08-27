package org.podval.tools.publish.page

import org.podval.tei.EntityKind
import org.podval.tools.publish.markup.Link
import org.podval.tools.publish.site.{Path, Posts, Site}
import org.podval.tools.publish.util.{Date, Icon}
import org.podval.xml.{Html, Xml}
import zio.blocks.html.*
import java.io.File
import java.net.URI
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

  def uri: URI = URI(
    "http",
    null,
    Site.localhost,
    site.httpServerPort,
    path.toString,
    null,
    null
  )
  
  final def targetFile: File = path.file(site.targetDirectory)

  def up: Option[Page] = parent

  // Not lazy: selector hops are known only after `Pages.resolveStores`.
  def parent: Option[DirectoryPage] =
    val parentDirectory: Option[Seq[String]] =
      if isDirectory && path.path.length > 1 then Some(path.path.init.init)
      else if !isDirectory && path.path.nonEmpty then Some(path.path.init)
      else None

    parentDirectory.flatMap(directoryParent)

  @scala.annotation.tailrec
  private def directoryParent(directory: Seq[String]): Option[DirectoryPage] =
    if directory.isEmpty then None
    else if site.pages.isSelectorHop(directory) then directoryParent(directory.init)
    else Some(site.pages.getOrAddDirectory(Path(directory :+ DirectoryPage.fileName *).html))

  def isAlias: Boolean = false

  def real: Page = this

  final def asFullMarkupPage: Option[FullMarkupPage] = this match
    case page: FullMarkupPage => Some(page)
    case _ => None

  def isDirectory: Boolean = false

  def source: Option[PageSource] = None

  // TODO not final: overridden in AssetWithSourcePath
  def sourcePath: Option[Path] = source.map(_.sourcePath)

  final def content: Option[PageContent] = source.map(_.content)
  final def content[A](f: PageContent => Option[A]): Option[A] = content.flatMap(f)
  
  protected def frontMatter: FrontMatter = content.fold(FrontMatter.absent)(_.frontMatter)

  final lazy val postDate: Option[LocalDate] = Posts.date(path)
  final def isPost: Boolean = postDate.isDefined || frontMatter.post // TODO take permalink into account?
  final def date: Option[Date] = postDate.map(Date.Local(_)).orElse(content(_.frontMatter.date))
  final def dateModified: Option[Date] = content(_.frontMatter.modifiedTime)
  final def dateModifiedGit: Option[Instant] = sourcePath.map(_.toString).flatMap(site.git.modificationDate)

  final def title: String =
    content(_.title.map(_.getText))
    .orElse(content(_.frontMatter.title))
    .getOrElse(titleDefault)

  def titleDefault: String = titleFromPath
  def titleFromPath: String = path.fileName

  /** Directory listing label: store `name: title` when the child is a store; first TEI name for an entity. */
  final def listTitle: String =
    entityDisplayName.getOrElse:
      content.flatMap(_.storeIndex).flatMap(_.displayName) match
        case Some(name) =>
          val t: String = title.trim
          if t.isEmpty || t == name || t == titleFromPath then name
          else s"$name: $t"
        case None =>
          title

  final def listRef(cls: Option[String] = None): Html.Element =
    val pageLink: Link = Link(this, fragment = None, isIntrapage = false)
    val clss = (Seq("page-ref") ++ cls.toSeq).mkString(" ")
    a(
      className := clss,
      href := pageLink.url,
      this.icon.html,
      listTitle
    )

  final def description: Option[String] = content(_.frontMatter.description).orElse(descriptionDefault)
  protected def descriptionDefault: Option[String] = None

  final def icon: Icon = frontMatterIcon.getOrElse(iconDefault)
  private def frontMatterIcon: Option[Icon] = frontMatter
    .icon
    .map(icon => Icon(icon, frontMatter.iconStyle.getOrElse(Icon.Regular)))

  protected def iconDefault: Icon

  final def entityKind: Option[EntityKind] = content(content =>
    EntityKind.values.find(entityKind => content.xml.getName == entityKind.element)
  )

  final def entityRole: Option[String] =
    content(c => c.xml.get("role").map(_.trim).filter(_.nonEmpty))

  final def entityDisplayName: Option[String] =
    entityKind.flatMap: kind =>
      content.flatMap: c =>
        c.xml.getChildren.flatMap(_.asElement)
          .find(el => el.getName == kind.nameElement || el.hasClass(kind.nameElement))
          .map(_.getText.trim)
          .filter(_.nonEmpty)
  
  final def ref(
    cls: Option[String] = None,
    withTitle: Boolean = true,
    withIcon: Boolean = true,
    icon: Option[Icon] = None
  ): Html.Element =
    val clss = (Seq("page-ref") ++ cls.toSeq).mkString(" ")
    val pageLink: Link = Link(this, fragment = None, isIntrapage = false)
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
