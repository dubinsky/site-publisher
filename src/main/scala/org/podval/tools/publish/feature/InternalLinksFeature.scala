package org.podval.tools.publish.feature

import org.podval.tools.publish.page.Page
import org.podval.tools.publish.PageError
import org.podval.tools.publish.link.{Link, LinkKind}
import org.podval.xml.{HtmlClass, Xml}
import java.net.{URI, URISyntaxException}

object InternalLinksFeature extends Feature(
  processesLinks = true
):
  override def process(
    element: Xml.Element,
    context: Feature.ProcessContext
  ): Xml.Element =
    if !element.isA then element else
      element.getHref.fold(element): href =>
        // TODO verify that external link is not broken if the Site is so configured
        val isInternal: Boolean =
          try
            val uri: URI = URI(href)
            if uri.getScheme != null && uri.getHost == context.siteUrl
            then context.errorReporter.error(PageError.SelfLink, href, None)
            uri.getScheme == null
          catch case e: URISyntaxException => true

        if !isInternal
        then element
        else Links.markInternalLink(element)

  override def postProcess(
    element: Xml.Element,
    context: Feature.PostProcessContext

  ): Xml.Element =
    if !element.isA || !Links.isInternalLink(element)
    then element
    else element.getHref.fold(element)(resolveInternalLinks(element, context.page, context.errorReporter, _))

  private def resolveInternalLinks(
    element: Xml.Element,
    page: Page,
    errorReporter: PageError.Reporter,
    ref: String
  ): Xml.Element =
    val kind: Option[LinkKind] = LinkKind.of(element)
    Link.resolve(ref, kind, page) match
      case None =>
        errorReporter.error(PageError.Unresolved, s"unresolved internal link '$ref' of kind $kind: $element", element)
        element.add(HtmlClass("unresolved-link")) // TODO move into Links
      case Some(linkTo) =>
        // TODO transclude
        val result: Xml.Element = element.setHref(linkTo.url)

        if result.getText != Links.linkText(element, ref)
        then result
        else result.setText(Links.linkText(element, linkTo.title))
