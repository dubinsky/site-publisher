package org.podval.tools.publish.markup

import org.podval.tools.publish.processor.SingleProcessor
import org.podval.xml.XmlDialect

// Markup that parses into XML.
// Sections are nested.
abstract class XmlLikeMarkup(
  xmlDialect: XmlDialect,
  processors: Seq[SingleProcessor]
) extends Markup(
  xmlDialect,
  processors
) with XmlParsableMarkup

object XmlLikeMarkup:
  val extension: String = "xml"
