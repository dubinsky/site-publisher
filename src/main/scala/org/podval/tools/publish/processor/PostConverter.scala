package org.podval.tools.publish.processor

import org.podval.tools.publish.page.PageContent
import org.podval.xml.Xml

// Converts individual XML elements.
trait PostConverter extends Processor:
  def postConvert(
    element: Xml.Element,
    content: PageContent
  ): Xml.Element
