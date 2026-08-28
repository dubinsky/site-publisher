package org.podval.tools.publish.markup

import org.podval.tools.publish.page.{Page, PageContent}
import org.podval.tools.publish.site.Path
import org.podval.tools.publish.util.Strings
import org.podval.xml.{HtmlClass, Xml}

final class Link(
  val page: Page,
  val fragment: Option[Link.ToFragment],
  val isIntrapage: Boolean
):
  def url: String = withFragment(page.real.site.pages.publishedPath(page.real).toString, _.id)
  def title: String = withFragment(page.title, _.title)
  def titleReal: String = withFragment(page.real.title, _.title)

  private def withFragment(
    fromPage: String,
    get: Link.ToFragment => String
  ): String = fragment match
    case None => s"$fromPage" // Note: not just "#", to deal with the possibility of being chunked
    case Some(fragment) => (if isIntrapage then "" else fromPage) + s"#${get(fragment)}"

object Link:
  object InternalLinkClass extends HtmlClass("internal-link")

  object UnresolvedLinkClass extends HtmlClass("unresolved-link")

  def isInternal(element: Xml.Element): Boolean = element.isA && element.has(InternalLinkClass)

  sealed abstract class ToFragment:
    def title: String
    def id: String

  final class ToBlock(block: WikiBlock) extends ToFragment:
    override def id: String = block.id
    override def title: String = s"^${block.id}"

  final class ToSection(sections: Seq[Section]) extends ToFragment:
    override def id: String = sections.last.id
    override def title: String = sections.map(_.title).mkString("#")

  final class ToId(override val id: String) extends ToFragment:
    override def title: String = s"#$id"

  // path could be `name`, `path/name`(?) - or empty, for intrapage links.
  // fragment could be `#section`, `#section#subsection`, `#^block`, or #id.
  def resolve(
    ref: String,
    kind: Option[LinkKind],
    from: Page
  ): Option[Link] =
    val (pathStringRaw: String, fragmentStr: Option[String]) = Strings.splitFirst(ref, '#')
    val pathString: String = pathStringRaw.trim
    val isAbsolute: Boolean = pathString.startsWith("/")
    val isFileHref: Boolean = isAbsolute || Path.isRelativeFileHref(pathString)
    val path: Path =
      if pathString.isEmpty then Path.root
      else if isFileHref then from.path.resolveFrom(pathString)
      else Path.fromHref(pathString)

    val to: Option[Page] =
      if pathString.isEmpty
      then Some(from)
      else from.site.pages.find(path, isFileHref, kind)

    to.map: to =>
      val fragment: Option[ToFragment] = fragmentStr.flatMap: fragment =>
        val content: Option[PageContent] = to.real.content
        if fragment.startsWith("^")
        then content.flatMap(_.blocks.resolve(id = fragment.substring(1).trim))
        else if fragment.contains("#")
        then content.map(_.toc).flatMap(_.resolveSection(names = fragment.split('#').map(_.trim).toSeq))
        else content.flatMap(_.ids.resolve(fragment)).orElse(
          content.map(_.toc).flatMap(_.resolveSection(names = Seq(fragment.trim)))
        )

      Link(
        page = to,
        isIntrapage = from == to,
        fragment = fragment
      )
