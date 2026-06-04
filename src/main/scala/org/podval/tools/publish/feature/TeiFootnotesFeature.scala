package org.podval.tools.publish.feature

import org.podval.tools.publish.page.PageSource
import org.podval.tools.publish.processor.{Converter, Feature}
import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.{Xml, XmlAttribute}

final class TeiFootnotesFeature extends Feature(
  converter = Some(TeiFootnotesFeature.TeiFootnotesConverter())
)

object TeiFootnotesFeature:
  private final class TeiFootnotesConverter extends Converter:
    override def convert(
      element: Xml.Element,
      pageSource: PageSource,
      ids: IdGenerator,
      footnoteCorrelationIds: IdGenerator
    ): Xml.Element =
      val isFootnote: Boolean = element.getName == "note" && element.get(XmlAttribute("place")).contains("end")
      if !isFootnote
      then element
      else Footnotes.linkAndBodyStub(element, footnoteCorrelationIds.generate())
