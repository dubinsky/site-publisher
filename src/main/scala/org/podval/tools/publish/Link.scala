package org.podval.tools.publish

import org.podval.tools.publish.util.{Files, Strings}

final class Link(
  val page: Page,
  fragment: Option[Link.ToFragment],
  intrapage: Boolean,
  alias: Option[String] = None
):
  def url: String = withFragment(page.path.toString, _.id)
  def title: String = withFragment(alias.getOrElse(page.title), _.title)

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
  def resolve(ref: String, from: Page): Option[Link] =
    val (pathString: String, fragment: Option[String]) = Strings.split(ref, '#')

    val to: Option[(Page, Option[String])] =
      if pathString.trim.isEmpty then Some(from, None) else
        val isAbsolute: Boolean = pathString.trim.startsWith("/")
        val pathSegments: Seq[String] = pathString.trim.split('/').toSeq.filterNot(_.isEmpty).map(_.trim)
        val path: Path = if pathSegments.isEmpty then Path.root else
          val (lastSegment, extension) = Files.nameAndExtension(pathSegments.last)
          Path(pathSegments.init :+ lastSegment.trim, extension)

        from.site.pages.flatMap(page => is(page, path, isAbsolute)).headOption

    to.map((to, alias) => Link(
      page = to,
      intrapage = from == to,
      alias = alias,
      fragment = fragment.flatMap: fragment =>
        val toc: Option[Toc] = to.source.map(_.cached.toc)
        if fragment.startsWith("^")
        then toc.flatMap(_.resolveBlock(id = fragment.substring(1).trim))
        else toc.flatMap(_.resolveSection(names = fragment.split('#').map(_.trim).toSeq)).orElse(
          toc.flatMap(_.resolveId(fragment))
        )
    ))

  private def is(page: Page, path: Path, isAbsolute: Boolean): Option[(Page, Option[String])] =
    isPath(page, path, isAbsolute).orElse(
      Option.when(page.sourcePath.exists(isSourcePath(_, path, isAbsolute)))((page, None))
    )

  private def isPath(page: Page, path: Path, isAbsolute: Boolean): Option[(Page, Option[String])] =
    def loop(current: Page, names: Seq[String]): Option[(Page, Option[String])] =
      val name: String = names.head
      val to: Option[(Page, Option[String])] =
        if current.path.fileName == name then Some(page, None)
        else if current.title == name then Some(page, None)
        else if current.aliases.contains(name) then Some(page, Some(name))
        else None

      val tail: Seq[String] = names.tail
      val done: Boolean = tail.isEmpty
      to.flatMap: (to: (Page, Option[String])) =>
        current.parent match
          case None =>
            Option.when(done)(to)
          case Some(parent) =>
            if done
            then Option.when(!isAbsolute)(to)
            else loop(parent, tail)

    if !isExtension(page.path, path) then None else loop(page, path.path)

  private def isSourcePath(sourcePath: Path, path: Path, isAbsolute: Boolean): Boolean =
    isExtension(sourcePath, path) && (
      if isAbsolute
      then sourcePath.path == path.path
      else sourcePath.path.endsWith(path.path)
    )

  private def isExtension(pagePath: Path, path: Path): Boolean =
    path.extension.fold(true)(pagePath.extension.contains)
