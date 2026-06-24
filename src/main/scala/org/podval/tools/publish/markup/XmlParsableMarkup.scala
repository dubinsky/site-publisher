package org.podval.tools.publish.markup

trait XmlParsableMarkup extends MarkupKind:
  final override def xmlContent(content: String): String = content
