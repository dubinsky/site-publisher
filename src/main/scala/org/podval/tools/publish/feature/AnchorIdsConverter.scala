package org.podval.tools.publish.feature

import org.podval.tools.publish.page.PageContent
import org.podval.tools.publish.processor.Converter
import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.Xml

final class AnchorIdsConverter extends Converter(convertLinks = true):
  override def convert(
    element: Xml.Element,
    content: PageContent,
    ids: IdGenerator,
    footnoteCorrelationIds: IdGenerator
  ): Xml.Element =
    if !element.isA || element.getId.isDefined
    then element
    else element.setId(ids.generate())
