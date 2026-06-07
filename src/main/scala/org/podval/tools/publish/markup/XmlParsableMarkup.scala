package org.podval.tools.publish.markup

trait XmlParsableMarkup extends Markup:
  final override def xmlContent(content: String): String = content
