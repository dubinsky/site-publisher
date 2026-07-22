package org.podval.tools.publish.markup
import org.podval.tools.publish.{Path, Site}

trait XmlParsableMarkup extends MarkupKind:
  final override def xmlContent(
    site: Site,
    sourcePath: Path,
    content: String
  ): String =
    content
