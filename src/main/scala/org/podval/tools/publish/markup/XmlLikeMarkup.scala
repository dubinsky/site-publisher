package org.podval.tools.publish.markup

// Markup that parses into XML.
// Sections are nested.
abstract class XmlLikeMarkup extends XmlParsableMarkup:
  final override val extension: String = XmlLikeMarkup.extension
  final def name: String = xmlDialect.name

object XmlLikeMarkup:
  val extension: String = "xml"
