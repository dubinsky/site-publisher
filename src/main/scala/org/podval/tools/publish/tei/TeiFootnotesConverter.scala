package org.podval.tools.publish.tei

import org.podval.tools.publish.markup.{Converter, Footnotes}
import org.podval.tools.publish.page.PageSource
import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.Xml

final class TeiFootnotesConverter extends Converter:
  override protected def convert(
    element: Xml.Element,
    source: PageSource,
    ids: IdGenerator,
    footnoteCorrelationIds: IdGenerator
  ): Option[Xml.Element] =
    Option.when(element.getName == "note" && element.get("place").contains("end"))(
      Footnotes.linkAndBodyStub(element, footnoteCorrelationIds.generate())
    )
