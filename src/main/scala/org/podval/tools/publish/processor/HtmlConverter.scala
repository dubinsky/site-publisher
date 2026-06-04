package org.podval.tools.publish.processor

import org.podval.tools.publish.page.PageSource
import org.podval.xml.Html

// Converts individual HTML elements.
trait HtmlConverter extends Processor:
  def convertHtml(
    element: Html.Element,
    pageSource: PageSource
  ): Html.Element
