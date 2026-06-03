package org.podval.tools.publish.features

import org.podval.tools.publish.{Link, Page, PageError}
import org.podval.xml.{HtmlAttribute, HtmlClass, HtmlElement, Xml}
import java.net.{URI, URISyntaxException}

object InternalLinksFeature extends Feature(
  // Note: process this last, so that everything that was to be converted to a link had:
  processPriority = 100
):
  object InternalLinkClass extends HtmlClass("internal-link")

  override def process(
    element: Xml.Element,
    context: Feature.ProcessContext
  ): Xml.Element =
    if !element.isElement(HtmlElement.A) then element else
      element.get(HtmlAttribute.Href).fold(element): href =>
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
        else element.add(InternalLinksFeature.InternalLinkClass)

  override def postProcess(
    element: Xml.Element,
    context: Feature.PostProcessContext

  ): Xml.Element =
    if !element.isElement(HtmlElement.A) || !element.has(InternalLinkClass)
    then element
    else element.get(HtmlAttribute.Href).fold(element)(resolveInternalLinks(element, context.page, context.errorReporter, _))

  private def resolveInternalLinks(
    element: Xml.Element,
    page: Page,
    errorReporter: PageError.Reporter,
    ref: String
  ): Xml.Element =
    val kind: Option[Link.Kind] = Link.Kind.of(element)
    Link.resolve(ref, kind, page) match
      case None =>
        errorReporter.error(PageError.Unresolved, s"unresolved internal link '$ref' of kind $kind: $element", element)
        element.add(HtmlClass("unresolved-link"))
      case Some(linkTo) =>
        // TODO transclude
        val result: Xml.Element = element.set(HtmlAttribute.Href, linkTo.url)

        def linkText(text: String): String =
          if element.has(WikiLinksFeature.WikiLinkClass)
          then WikiLinksFeature.wikiLinkText(element.has(WikiLinksFeature.TranscludeClass), text)
          else text

        if result.getText != linkText(ref)
        then result
        else result.setText(linkText(linkTo.title))
