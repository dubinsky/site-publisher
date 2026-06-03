package org.podval.tools.publish.features

import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.{Xml, XmlAttribute}

object TeiFootnotesFeature extends Feature:

  override def transform(
    element: Xml.Element,
    context: Feature.TransformContext
  ): Xml.Element =
    val correlationIds: IdGenerator = IdGenerator("")

    context.xmlDialect.transform(element, element =>
      val isFootnote = element.getName == "note" && element.get(XmlAttribute("place")).contains("end")
      if !isFootnote then element else element
        .set(Footnotes.CorrelationId, correlationIds.generate())
        .add(Footnotes.LinkClass)
        .add(Footnotes.BodyClass)
    )
