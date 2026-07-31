package org.podval.tools.publish.tei

import org.podval.tools.publish.markup.{Converter, Footnotes}
import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.Xml

// TODO split into body and link stubs - and simplify the non-markup-specific footnotes processing!
final class TeiFootnotesConverter(ids: IdGenerator) extends Converter:
  override def convert(element: Xml.Element): Option[Xml.Element] =
    Option.when(element.getName == "note" && element.get("place").contains("end"))(
      Footnotes.linkAndBodyStub(element, ids.footnoteCorrelationId())
    )
