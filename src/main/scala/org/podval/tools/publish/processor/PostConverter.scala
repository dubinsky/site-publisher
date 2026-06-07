package org.podval.tools.publish.processor

import org.podval.tools.publish.page.PageContent
import org.podval.xml.Xml

// Converts individual XML elements.
abstract class PostConverter extends SingleProcessor:
  def postConvert(
    element: Xml.Element,
    content: PageContent
  ): Xml.Element
