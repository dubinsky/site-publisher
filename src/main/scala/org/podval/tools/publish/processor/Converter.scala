package org.podval.tools.publish.processor

import org.podval.tools.publish.page.PageContent
import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.Xml

abstract class Converter(
  convertLinks: Boolean = false
) extends ConverterWithIds(
  convertLinks
):
  final override def convertWithIds(
    element: Xml.Element,
    content: PageContent,
    ids: IdGenerator,
    footnoteCorrelationIds: IdGenerator
  ): Xml.Element = convert(
    element,
    content
  )
  
  protected def convert(
    element: Xml.Element,
    content: PageContent
  ): Xml.Element
