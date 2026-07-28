package org.podval.tools.publish.link

import org.podval.tools.publish.page.{Page, PageContent}
import org.podval.tools.publish.util.{Files, Strings}
import org.podval.tools.publish.Path

final class Link(
  val page: Page,
  fragment: Option[Link.ToFragment],
  intrapage: Boolean
):
  def url: String = withFragment(page.real.path.toString, _.id)
  def title: String = withFragment(page.title, _.title)
  def titleReal: String = withFragment(page.real.title, _.title)

  private def withFragment(
    fromPage: String,
    get: Link.ToFragment => String
  ): String =
    (if intrapage then "" else fromPage) +
    fragment.fold("")(fragment => s"#${get(fragment)}")

object Link:
  sealed abstract class ToFragment:
    def title: String
    def id: String

  final class ToBlock(block: Fragment.Block) extends ToFragment:
    override def id: String = block.id
    override def title: String = s"^${block.id}"

  final class ToSection(sections: Seq[Fragment.Section]) extends ToFragment:
    override def id: String = sections.last.id
    override def title: String = sections.map(_.title).mkString("#")

  final class ToId(override val id: String) extends ToFragment:
    override def title: String = s"#$id"

  // path could be `name`, `path/name`(?) - or empty, for intrapage links.
  // fragment could be `#section`, `#section#subsection`, `#^block`, or #id.
  def resolve(ref: String, kind: Option[LinkKind], from: Page): Option[Link] =
    val (pathStringRaw: String, fragment: Option[String]) = Strings.splitFirst(ref, '#')
    val pathString: String = pathStringRaw.trim
    // TODO unify with Path.relativize()
    val isAbsolute: Boolean = pathString.startsWith("/")
    val pathSegments: Seq[String] = pathString.split('/').toSeq.filterNot(_.isEmpty).map(_.trim)
    val path: Path = if pathSegments.isEmpty then Path.root else
      val (lastSegment, extension) = Files.nameAndExtension(pathSegments.last)
      Path(pathSegments.init :+ lastSegment.trim, extension)

    val to: Option[Page] =
      if pathString.isEmpty
      then Some(from)
      else from.site.pages.find(path, isAbsolute, kind)

    to.map(to => Link(
      page = to,
      intrapage = from == to,
      fragment = fragment.flatMap: fragment =>
        val content: Option[PageContent] = to.real.content
        if fragment.startsWith("^")
        then content.flatMap(_.resolveBlock(id = fragment.substring(1).trim))
        else content.map(_.toc).flatMap(_.resolveSection(names = fragment.split('#').map(_.trim).toSeq)).orElse(
          content.flatMap(_.resolveId(fragment))
        )
    ))
