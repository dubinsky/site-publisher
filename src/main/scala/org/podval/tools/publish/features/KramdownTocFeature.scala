package org.podval.tools.publish.features

import org.podval.xml.Html

// Add TOC in place of the marker
object KramdownTocFeature extends Feature:

  override def postProcessHtml(
    element: Html.Element,
    context: Feature.PostProcessHtmlContext
  ): Html.Element =
    if !isKramdownTocMarker(element)
    then element
    else context.toc.html
  
  private def isKramdownTocMarker(element: Html.Element): Boolean =
    element.getName == "ul" && element.getChildren.exists: node =>
      node.asElement.fold(false): child =>
        child.getName == "li" &&
        child.getChildren.length == 1 &&
        child.getChildren.head.asText.fold(false): text =>
          text.endsWith("{:toc}")
   