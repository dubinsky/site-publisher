package org.podval.tools.publish.processor

import org.podval.tools.publish.page.PageContent
import org.podval.xml.Xml

// Converts individual XML elements.
abstract class PostConverter extends Processor:
  def postConvert(
    element: Xml.Element,
    content: PageContent
  ): Option[Xml.Element] = postConvert(
    element
  )

  protected def postConvert(element: Xml.Element): Option[Xml.Element] =
    None
