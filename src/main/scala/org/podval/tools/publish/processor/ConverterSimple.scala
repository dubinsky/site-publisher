package org.podval.tools.publish.processor

import org.podval.tools.publish.page.PageContent
import org.podval.xml.Xml

abstract class ConverterSimple extends Converter:
  final override protected def convert(
    element: Xml.Element,
    content: PageContent
  ): Xml.Element = convert(
    element
  )
  
  protected def convert(element: Xml.Element): Xml.Element
