package org.podval.tools.publish.feature

import org.podval.tools.publish.page.PageContent
import org.podval.tools.publish.processor.Converter
import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.{Xml, XmlAttribute}

final class TeiFootnotesConverter extends Converter:
  override def convert(
    element: Xml.Element,
    content: PageContent,
    ids: IdGenerator,
    footnoteCorrelationIds: IdGenerator
  ): Xml.Element =
    val isFootnote: Boolean = element.getName == "note" && element.get(XmlAttribute("place")).contains("end")
    if !isFootnote
    then element
    else Footnotes.linkAndBodyStub(element, footnoteCorrelationIds.generate())
