package org.podval.tools.publish.markup

import org.podval.tools.publish.page.{OriginalMarkupPage, Page, PageContent}
import org.podval.tools.publish.site.{PageError, PageErrorReporter, Path, Site}
import org.podval.tools.publish.util.{Files, IdGenerator, Strings}
import org.podval.xml.{HtmlClass, Xml, XmlDialect}
import java.net.{URI, URISyntaxException}

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
  object InternalLinkClass extends HtmlClass("internal-link")

  private object UnresolvedLinkClass extends HtmlClass("unresolved-link")

  def setAnchorId(element: Xml.Element, ids: IdGenerator): Option[Xml.Element] =
    Option.when(element.isA && element.getId.isEmpty)(
      element.setId(ids.generate())
    )

  def markInternal(
    element: Xml.Element,
    site: Site,
    errorReporter: PageErrorReporter
  ): Option[Xml.Element] =
    if !element.isA then None else
      element.getHref.flatMap: href =>
        // TODO verify that external link is not broken if the Site is so configured
        val isInternal: Boolean =
          try
            val uri: URI = URI(href)
            if site.isSelf(uri) then errorReporter.error(PageError.SelfLink, href)
            uri.getScheme == null
          catch case e: URISyntaxException => true

        Option.when(isInternal)(
          element.add(InternalLinkClass)
        )

  sealed abstract class ToFragment:
    def title: String
    def id: String

  final class ToBlock(block: Block) extends ToFragment:
    override def id: String = block.id
    override def title: String = s"^${block.id}"

  final class ToSection(sections: Seq[Section]) extends ToFragment:
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
        then content.flatMap(_.blocks.resolve(id = fragment.substring(1).trim))
        else content.map(_.toc).flatMap(_.resolveSection(names = fragment.split('#').map(_.trim).toSeq)).orElse(
          content.flatMap(_.ids.resolve(fragment))
        )
    ))

  def resolveInternalLinks(
    xml: Xml.Element,
    xmlDialect: XmlDialect,
    page: OriginalMarkupPage,
    errorReporter: PageErrorReporter
  ): Xml.Element =
    def resolveInternalLink(
      element: Xml.Element,
      ref: String
    ): Xml.Element =
      val kind: Option[LinkKind] = LinkKind.of(element)
      resolve(ref, kind, page) match
        case None =>
          errorReporter.error(PageError.Unresolved, s"unresolved internal link '$ref' of kind $kind: $element")
          element.add(UnresolvedLinkClass)
        case Some(linkTo) =>
          // TODO transclude
          val result: Xml.Element = element.setHref(linkTo.url)

          if result.getText != WikiLink.linkText(element, ref)
          then result
          else result.setText(WikiLink.linkText(element, linkTo.title))

    xmlDialect.transform(xml, element => Option
      .when(element.isA && element.has(InternalLinkClass))(
        element.getHref.fold(element)(ref => resolveInternalLink(element, ref))
      )
      .getOrElse(element)
    )
