package org.podval.tools.publish.markup

import org.podval.xml.{Xml, XmlDialect}

// Replace footnotes with link stubs
final class FootnoteLinksTransformer(xmlDialect: XmlDialect) extends Transformer:
  override def transform(element: Xml.Element): Xml.Element =
    xmlDialect.transform(element, element =>
      Footnotes.getCorrelationId(element).fold(element)(Footnotes.linkStub)
    )
    