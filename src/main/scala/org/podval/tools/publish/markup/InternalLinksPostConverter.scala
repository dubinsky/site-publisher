package org.podval.tools.publish.markup

import org.podval.tools.publish.PageError
import org.podval.tools.publish.link.{Link, LinkKind}
import org.podval.tools.publish.markup.Links
import org.podval.tools.publish.page.PageSource
import org.podval.tools.publish.processor.PostConverter
import org.podval.xml.Xml

final class InternalLinksPostConverter extends PostConverter:
  override def postConvert(
    element: Xml.Element,
    source: PageSource
  ): Option[Xml.Element] =
    Option.when(element.isA && Links.isInternalLink(element))(
      element.getHref.fold(element)(resolveInternalLinks(element, source, _))
    )

private def resolveInternalLinks(
  element: Xml.Element,
  source: PageSource,
  ref: String
): Xml.Element =
  val kind: Option[LinkKind] = LinkKind.of(element)
  Link.resolve(ref, kind, source.page) match
    case None =>
      source.error(PageError.Unresolved, s"unresolved internal link '$ref' of kind $kind: $element")
      element.addClass("unresolved-link") // TODO move into Links
    case Some(linkTo) =>
      // TODO transclude
      val result: Xml.Element = element.setHref(linkTo.url)

      if result.getText != Links.linkText(element, ref)
      then result
      else result.setText(Links.linkText(element, linkTo.title))
