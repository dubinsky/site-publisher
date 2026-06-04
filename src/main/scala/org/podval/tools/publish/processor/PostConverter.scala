package org.podval.tools.publish.processor

import org.podval.tools.publish.page.PageSource
import org.podval.xml.Xml

// Converts individual XML elements.
trait PostConverter extends Processor:
  def postConvert(
    element: Xml.Element,
    pageSource: PageSource
  ): Xml.Element
