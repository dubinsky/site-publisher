package org.podval.tools.publish.markup

import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.Xml

final class AnchorIdsConverter(ids: IdGenerator) extends Converter:
  override protected def convert(element: Xml.Element): Option[Xml.Element] =
    Option.when(element.isA && element.getId.isEmpty)(
      element.setId(ids.generate())
    )
