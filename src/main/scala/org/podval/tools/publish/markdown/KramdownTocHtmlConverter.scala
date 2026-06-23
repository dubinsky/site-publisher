package org.podval.tools.publish.markdown

import org.podval.tools.publish.page.PageContent
import org.podval.tools.publish.processor.HtmlConverter
import org.podval.xml.Html

// Add TOC in place of the marker
final class KramdownTocHtmlConverter extends HtmlConverter:
  override def convertHtml(
    element: Html.Element,
    content: PageContent
  ): Html.Element =
    if !isKramdownTocMarker(element)
    then element
    else content.toc.html

private def isKramdownTocMarker(element: Html.Element): Boolean =
  element.getName == "ul" && element.getChildren.exists: node =>
    node.asElement.fold(false): child =>
      child.getName == "li" &&
      child.getChildren.length == 1 &&
      child.getChildren.head.asText.fold(false): text =>
        text.endsWith("{:toc}")
