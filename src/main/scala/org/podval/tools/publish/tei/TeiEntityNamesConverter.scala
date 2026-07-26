package org.podval.tools.publish.tei

import org.podval.tei.EntityKind
import org.podval.tools.publish.processor.Converter
import org.podval.xml.Xml

final class TeiEntityNamesConverter extends Converter:
  override protected def convert(element: Xml.Element): Option[Xml.Element] =
    // TODO turn those into As *only* if 'ref' attribute is present!
    Option.when(EntityKind.values.exists(_.nameElement == element.getName))(
      renameElement("a", copyAttribute("ref", "href", element))
    )
