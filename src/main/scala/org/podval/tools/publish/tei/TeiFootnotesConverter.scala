package org.podval.tools.publish.tei

import org.podval.tools.publish.markup.Footnotes
import org.podval.tools.publish.page.PageContent
import org.podval.tools.publish.processor.Converter
import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.Xml

final class TeiFootnotesConverter extends Converter:
  override def convert(
    element: Xml.Element,
    content: PageContent,
    ids: IdGenerator,
    footnoteCorrelationIds: IdGenerator
  ): Option[Xml.Element] =
    Option.when(element.getName == "note" && element.get("place").contains("end"))(
      Footnotes.linkAndBodyStub(element, footnoteCorrelationIds.generate())
    )
