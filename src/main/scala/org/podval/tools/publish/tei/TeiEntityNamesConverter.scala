package org.podval.tools.publish.tei

import org.podval.tei.EntityKind
import org.podval.tools.publish.processor.ConverterSimple
import org.podval.xml.Xml

final class TeiEntityNamesConverter extends ConverterSimple:
  override def convert(element: Xml.Element): Xml.Element =
    // TODO turn those into As *only* if 'ref' attribute is present!
    if !EntityKind.names.contains(element.getName)
    then element
    else renameElement("a", copyAttribute("ref", "href", element))
