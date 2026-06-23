package org.podval.tools.publish.markup

import org.podval.tools.publish.page.PageContent
import org.podval.tools.publish.processor.ConverterWithIds
import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.Xml

final class AnchorIdsConverter extends ConverterWithIds(convertLinks = true):
  override def convertWithIds(
    element: Xml.Element,
    content: PageContent,
    ids: IdGenerator,
    footnoteCorrelationIds: IdGenerator
  ): Xml.Element =
    if !element.isA || element.getId.isDefined
    then element
    else element.setId(ids.generate())
