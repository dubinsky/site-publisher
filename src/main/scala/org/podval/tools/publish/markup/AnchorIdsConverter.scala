package org.podval.tools.publish.markup

import org.podval.tools.publish.page.PageContent
import org.podval.tools.publish.processor.Converter
import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.Xml

final class AnchorIdsConverter extends Converter:
  override def stage: Converter.Stage = Converter.Stage.Links

  override def convert(
    element: Xml.Element,
    content: PageContent,
    ids: IdGenerator,
    footnoteCorrelationIds: IdGenerator
  ): Option[Xml.Element] =
    Option.when(element.isA && element.getId.isEmpty)(
      element.setId(ids.generate())
    )
