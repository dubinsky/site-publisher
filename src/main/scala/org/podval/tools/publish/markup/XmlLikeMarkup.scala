package org.podval.tools.publish.markup

import org.podval.xml.XmlDialect

object XmlLikeMarkup:
  val extension: String = "xml"

// Markup that parses into XML.
// Sections are nested.
abstract class XmlLikeMarkup(
  name: String,
  xmlDialect: XmlDialect,
  additionalExtensions: Set[String] = Set.empty
) extends MarkupKind(
  name = name,
  xmlDialect = xmlDialect,
  allowsInternalFrontMatter = false,
  rendersToXml = true,
  extension = XmlLikeMarkup.extension,
  additionalExtensions = additionalExtensions
) with XmlParsableMarkup
