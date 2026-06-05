package org.podval.tools.publish.feature

import org.podval.tools.publish.page.{PageContent, PageSource}
import org.podval.tools.publish.PageError
import org.podval.tools.publish.link.{Link, LinkKind}
import org.podval.tools.publish.processor.{Converter, Feature, PostConverter}
import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.{HtmlClass, Xml}
import java.net.{URI, URISyntaxException}

final class InternalLinksFeature extends Feature(
  converter = Some(InternalLinksFeature.InternalLinksConverter()),
  postConverter = Some(InternalLinksFeature.InternalLinksPostConverter())
)
  
object InternalLinksFeature:
  private final class InternalLinksConverter extends Converter:
    override def runLast: Boolean = true

    override def convert(
      element: Xml.Element,
      source: PageSource,
      ids: IdGenerator,
      footnoteCorrelationIds: IdGenerator
    ): Xml.Element =
      if !element.isA then element else
        element.getHref.fold(element): href =>
          // TODO verify that external link is not broken if the Site is so configured
          val isInternal: Boolean =
            try
              val uri: URI = URI(href)
              if uri.getScheme != null && uri.getHost == source.site.config.url
              then source.errorReporter.error(PageError.SelfLink, href, None)
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
      else element.getHref.fold(element)(resolveInternalLinks(element, content.source, _))
  
    private def resolveInternalLinks(
      element: Xml.Element,
      source: PageSource,
      ref: String
    ): Xml.Element =
      val kind: Option[LinkKind] = LinkKind.of(element)
      Link.resolve(ref, kind, source.page) match
        case None =>
          source.errorReporter.error(
            PageError.Unresolved, s"unresolved internal link '$ref' of kind $kind: $element",
            element
          )
          element.add(HtmlClass("unresolved-link")) // TODO move into Links
        case Some(linkTo) =>
          // TODO transclude
          val result: Xml.Element = element.setHref(linkTo.url)
  
          if result.getText != Links.linkText(element, ref)
          then result
          else result.setText(Links.linkText(element, linkTo.title))
