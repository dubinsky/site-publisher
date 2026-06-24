package org.podval.tools.publish.markup

import org.podval.tools.publish.PageError
import org.podval.tools.publish.link.{Link, LinkKind}
import org.podval.tools.publish.markup.Links
import org.podval.tools.publish.page.PageContent
import org.podval.tools.publish.processor.{Converter, PostConverter, Processors}
import org.podval.xml.Xml
import java.net.{URI, URISyntaxException}

final class InternalLinksProcessor extends Processors(
  new InternalLinksProcessor.InternalLinksConverter,
  new InternalLinksProcessor.InternalLinksPostConverter
)
  
private object InternalLinksProcessor:
  private final class InternalLinksConverter extends Converter(convertLinks = true):
    override protected def convert(
      element: Xml.Element,
      content: PageContent
    ): Xml.Element =
      if !element.isA then element else
        element.getHref.fold(element): href =>
          // TODO verify that external link is not broken if the Site is so configured
          val isInternal: Boolean =
            try
              val uri: URI = URI(href)
              if uri.getScheme != null && uri.getHost == content.site.url
              then content.error(PageError.SelfLink, href)
              uri.getScheme == null
            catch case e: URISyntaxException => true
  
          if !isInternal
          then element
          else Links.markInternalLink(element)

  private final class InternalLinksPostConverter extends PostConverter:
    override def postConvert(
      element: Xml.Element,
      content: PageContent
    ): Xml.Element =
      if !element.isA || !Links.isInternalLink(element)
      then element
      else element.getHref.fold(element)(resolveInternalLinks(element, content, _))
  
  private def resolveInternalLinks(
    element: Xml.Element,
    content: PageContent,
    ref: String
  ): Xml.Element =
    val kind: Option[LinkKind] = LinkKind.of(element)
    Link.resolve(ref, kind, content.page) match
      case None =>
        content.error(PageError.Unresolved, s"unresolved internal link '$ref' of kind $kind: $element")
        element.addClass("unresolved-link") // TODO move into Links
      case Some(linkTo) =>
        // TODO transclude
        val result: Xml.Element = element.setHref(linkTo.url)

        if result.getText != Links.linkText(element, ref)
        then result
        else result.setText(Links.linkText(element, linkTo.title))
