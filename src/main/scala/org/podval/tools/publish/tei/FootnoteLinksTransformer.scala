package org.podval.tools.publish.tei

import org.podval.tools.publish.markup.{Footnotes, Transformer}
import org.podval.xml.{Xml, XmlDialect}

// TODO this seems TEI-specific!
// Replace footnotes with link stubs
final class FootnoteLinksTransformer(xmlDialect: XmlDialect) extends Transformer:
  override def transform(element: Xml.Element): Xml.Element =
    xmlDialect.transform(element, element =>
      Footnotes.getCorrelationId(element).fold(element)(Footnotes.linkStub)
    )
    