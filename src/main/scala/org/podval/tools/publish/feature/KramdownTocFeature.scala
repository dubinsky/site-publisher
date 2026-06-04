package org.podval.tools.publish.feature

import org.podval.tools.publish.page.PageSource
import org.podval.tools.publish.processor.{Feature, HtmlConverter}
import org.podval.xml.Html

// Add TOC in place of the marker
final class KramdownTocFeature extends Feature(
  htmlConverter = Some(KramdownTocFeature.KramdownTocHtmlConverter())
)

object KramdownTocFeature:
  private final class KramdownTocHtmlConverter extends HtmlConverter:
    override def convertHtml(
      element: Html.Element,
      pageSource: PageSource
    ): Html.Element =
      if !isKramdownTocMarker(element)
      then element
      else pageSource.cached.toc.html

  private def isKramdownTocMarker(element: Html.Element): Boolean =
    element.getName == "ul" && element.getChildren.exists: node =>
      node.asElement.fold(false): child =>
        child.getName == "li" &&
        child.getChildren.length == 1 &&
        child.getChildren.head.asText.fold(false): text =>
          text.endsWith("{:toc}")
