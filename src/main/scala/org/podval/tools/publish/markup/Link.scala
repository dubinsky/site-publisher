package org.podval.tools.publish.markup

import org.podval.tools.publish.page.Page
import org.podval.xml.{HtmlClass, Xml}

final class Link(
  val page: Page,
  val fragment: Option[Link.ToFragment],
  val isIntrapage: Boolean
):
  def url: String = withFragment(page.real.publishedPath.toString, _.id)
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
