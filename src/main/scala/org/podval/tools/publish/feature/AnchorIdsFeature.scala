package org.podval.tools.publish.feature

import org.podval.tools.publish.page.PageSource
import org.podval.tools.publish.processor.{Converter, Feature}
import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.Xml

final class AnchorIdsFeature extends Feature(
  converter = Some(AnchorIdsFeature.AnchorIdsConverter())
)

object AnchorIdsFeature:
  private final class AnchorIdsConverter extends Converter:
    override def runLast: Boolean = true
    
    override def convert(
      element: Xml.Element,
      pageSource: PageSource,
      ids: IdGenerator,
      footnoteCorrelationIds: IdGenerator
    ): Xml.Element =
      if !element.isA || element.getId.isDefined
      then element
      else element.setId(ids.generate())
