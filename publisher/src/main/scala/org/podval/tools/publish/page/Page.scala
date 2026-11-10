package org.podval.tools.publish.page

import org.podval.metadata.Names
import org.podval.tools.publish.markup.{EntityKind, Link}
import org.podval.tools.publish.site.{Path, Posts, Site}
import org.podval.tools.publish.util.{Date, Http, Icon}
import org.podval.xml.Xml
import org.podval.xml.dsl.{*, given}
import java.io.File
import java.net.URI
import java.time.{Instant, LocalDate, OffsetDateTime}

abstract class Page(
  val site: Site,
  /** Four paths. There is no fifth URL type.
    * - `path` — file written under the target directory (`targetFile`).
    * - `sourcePath` — authored file, when there is one. Absent on a pure synthetic.
    *   `Posts.path` can make it differ from `path`: the written path changes during the scan.
    * - `publishedPath` — public href (`Pages.publishedPath`): unwrap `real`, then the longest
    *   `aliasDirectory` prefix (directory, store, or entity-lists), else `path`.
    *   A permalink is an `Alias` whose own `path` is the public file. `publishedPath` does not apply that alias again
    *   (`aliasDirectory` is `None` on the note).
    * - `real` — alias target. `Alias.real` unwraps. Every other page is `this`.
    * `Pages.rewriteRequest` is the inverse for the local server and the Worker: inbound URL to the written `path`
    * (alias prefix, then `/alias/facsimile/P`, then store-tree resolve).
    * Archive-path text (`PageHeader` crumb labels, Cite, backlink group headings) is not any of these URLs.
    */
  val path: Path
) derives CanEqual:
  lazy val names: Names = StoreTree.namesOf(this)

  // Identity intern for `StoreTree.node`. Path equality must not key this (`Stores.indexOf` uses `eq`).
  private[page] var nodeSlot: Option[StoreTree.PageNode] = None

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
    Http.localhost,
    site.httpServerPort,
    path.toString,
    null,
    null
  )
  
  final def targetFile: File = path.file(site.targetDirectory)

  /** Public href. The four path names are on `path`. */
  final def publishedPath: Path = site.pages.publishedPath(this)

  def up: Option[Page] = parent

  /** Directory segments that contain this page, after empty selector hops.
    * Empty means the site root. Not cached: hops are known only after `Pages.resolveStores`.
    */
  private[page] def containingDirectory: Seq[String] =
    val raw: Seq[String] =
      if isDirectory && path.path.length > 1 then path.path.init.init
      else if !isDirectory && path.path.nonEmpty then path.path.init
      else Seq.empty
    skipSelectorHops(raw)

  @scala.annotation.tailrec
  private def skipSelectorHops(directory: Seq[String]): Seq[String] =
    if directory.nonEmpty && site.pages.isSelectorHop(directory) then skipSelectorHops(directory.init)
    else directory

  // Not lazy: selector hops are known only after `Pages.resolveStores`.
  def parent: Option[DirectoryPage] =
    val directory: Seq[String] = containingDirectory
    if directory.isEmpty then None
    else Some(site.pages.getOrAddDirectory(Path(directory :+ DirectoryPage.fileName *).html))

  def isAlias: Boolean = false

  def real: Page = this

  def participation: PageParticipation = PageParticipation.default

  final def listedInDirectory: Boolean = participation.listedInDirectory

  final def leafIndex: Boolean = participation.leafIndex

  /** `#fragment` survives when this page's generated ids are not in the file.
    * Empty content is a separate clause in `Pages.resolve` and stays there.
    * One-list catalogs are ordinary full pages, so the check lives here, not on a subclass.
    */
  def keepUnauthoredFragment: Boolean =
    doc.flatMap(_.asEntityLists).exists: lists =>
      EntityLists.kept(lists.index, site.pages.pages).length == 1

  final def asFullMarkupPage: Option[FullMarkupPage] = this match
    case page: FullMarkupPage => Some(page)
    case _ => None

  def isDirectory: Boolean = false

  def source: Option[PageSource] = None

  // Note: not final: overridden in AssetWithSourcePath
  def sourcePath: Option[Path] = source.map(_.sourcePath)

  final def content: Option[PageContent] = source.map(_.content)
  final def content[A](f: PageContent => Option[A]): Option[A] = content.flatMap(f)
  final def doc: Option[Content] = content.map(_.doc)
  final def store: Option[StoreContent] = doc.flatMap(_.asStore)
  
  protected def frontMatter: FrontMatter = content.fold(FrontMatter.absent)(_.frontMatter)

  final lazy val postDate: Option[LocalDate] = Posts.date(path)

  // `_posts` path, `post: true`, or absolute permalink `/YYYY/MM/DD/title`.
  final def isPost: Boolean =
    postDate.isDefined || frontMatter.post || permalinkPostDate.isDefined

  final def date: Option[Date] =
    postDate.map(Date.Local(_))
      .orElse(content(_.frontMatter.date))
      .orElse(permalinkPostDate.map(Date.Local(_)))

  private def permalinkPostDate: Option[LocalDate] =
    frontMatter.permalink.map(_.trim).filter(_.startsWith("/")).flatMap: permalink =>
      Posts.date(path.relativize(permalink))

  final def dateModified: Option[Date] = content(_.frontMatter.modifiedTime)
  final def dateModifiedGit: Option[Instant] = sourcePath.map(_.toString).flatMap(site.git.modificationDate)

  /** `date` as an RFC 3339 instant in the site zone. */
  final def publishedAt: Option[OffsetDateTime] = date.map(Date.toOffsetDateTime(_, site.zone))

  /** Front matter `modified_time`, else the git commit time, else [[publishedAt]]. */
  final def updatedAt: Option[OffsetDateTime] =
    dateModified.map(Date.toOffsetDateTime(_, site.zone))
      .orElse(dateModifiedGit.map(site.toOffsetDateTime))
      .orElse(publishedAt)

  final def title: String =
    entityDisplayName
    .orElse(content(_.title.map(_.getText)))
    .orElse(content(_.frontMatter.title))
    .getOrElse(titleDefault)

  def titleDefault: String = titleFromPath
  def titleFromPath: String = path.fileName

  /** Directory listing label: store `name: title` when the child is a store; first TEI name for an entity. */
  final def listTitle: String =
    entityDisplayName.getOrElse:
      val named: Option[String] =
        Option.when(store.isDefined):
          val raw: String = names.doFind(site.languageSpec).name
          if store.exists(_.ref.isDefined) then site.languageSpec.language.get.sentenceCase(raw) else raw
        .orElse(doc.flatMap(_.listTitle))
      named match
        case Some(name) =>
          val t: String = title.trim
          if t.isEmpty || t == name || t == titleFromPath then name
          else s"$name: $t"
        case None =>
          title

  final def listRef(cls: Option[String] = None): Xml.Element =
    val pageLink: Link = Link(this, fragment = None, isIntrapage = false)
    a(
      className := "page-ref",
      cls.map(extra => className += extra),
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

  final def entityKind: Option[EntityKind] = doc.flatMap(_.entityKind)

  final def entityRole: Option[String] = doc.flatMap(_.entityRole)

  final def entityDisplayName: Option[String] = doc.flatMap(_.entityDisplayName)
  
  final def ref(
    cls: Option[String] = None,
    withTitle: Boolean = true,
    withIcon: Boolean = true,
    icon: Option[Icon] = None
  ): Xml.Element =
    val pageLink: Link = Link(this, fragment = None, isIntrapage = false)
    a(
      className := "page-ref",
      cls.map(extra => className += extra),
      href := pageLink.url,
      NamedWindows.targetAttr(this).map(name => target := name),
      Option.when(withIcon)(icon.getOrElse(this.icon).html),
      Option.when(withTitle)(pageLink.titleReal)
    )

  final def navRef(icon: Icon): Xml.Element = ref(
    cls = Some("nav-item"),
    icon = Some(icon),
    withTitle = false
  )

object Page:
  def pageList(pages: Seq[Page], cls: Option[String] = None): Xml.Element = ul(
    className := "page-list",
    pages.map(page => li(page.ref(cls = cls)))
  )
