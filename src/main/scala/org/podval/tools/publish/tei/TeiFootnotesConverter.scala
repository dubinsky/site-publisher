package org.podval.tools.publish.tei

import org.podval.tools.publish.markup.Footnotes
import org.podval.tools.publish.page.PageContent
import org.podval.tools.publish.processor.ConverterWithIds
import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.Xml

final class TeiFootnotesConverter extends ConverterWithIds:
  override def convertWithIds(
    element: Xml.Element,
    content: PageContent,
    ids: IdGenerator,
    footnoteCorrelationIds: IdGenerator
  ): Xml.Element =
    val isFootnote: Boolean = element.getName == "note" && element.get("place").contains("end")
    if !isFootnote
    then element
    else Footnotes.linkAndBodyStub(element, footnoteCorrelationIds.generate())
