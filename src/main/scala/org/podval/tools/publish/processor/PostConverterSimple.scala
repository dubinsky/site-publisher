package org.podval.tools.publish.processor

import org.podval.tools.publish.page.PageContent
import org.podval.xml.Xml

abstract class PostConverterSimple extends PostConverter:
  final def postConvert(
    element: Xml.Element,
    content: PageContent
  ): Xml.Element = postConvert(
    element
  )

  protected def postConvert(element: Xml.Element): Xml.Element
