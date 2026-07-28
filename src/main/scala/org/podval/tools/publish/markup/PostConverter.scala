package org.podval.tools.publish.markup

import org.podval.tools.publish.page.PageSource
import org.podval.xml.Xml

// Converts individual XML elements.
abstract class PostConverter extends Processor:
  def postConvert(
    element: Xml.Element,
    source: PageSource
  ): Option[Xml.Element] = postConvert(
    element
  )

  protected def postConvert(element: Xml.Element): Option[Xml.Element] =
    None
