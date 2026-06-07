package org.podval.tools.publish.processor

import org.podval.tools.publish.page.PageContent
import org.podval.xml.Html

// Converts individual HTML elements.
abstract class HtmlConverter extends SingleProcessor:
  def convertHtml(
    element: Html.Element,
    content: PageContent
  ): Html.Element
