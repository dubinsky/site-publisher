package org.podval.tools.publish.page

import org.podval.tools.publish.markup.{Footnote, Link, Markup, Toc, WikiLink}
import org.podval.xml.{Xml, XmlDialect}

final class PageContentResolved private(
  val source: PageSource,
  val xml: Xml.Element,
  val toc: Toc,
  val footnotes: Map[String, Footnote]
)

object PageContentResolved:
  def apply(content: PageContent): PageContentResolved =
    val markup: Markup = content.source.markup
    val xmlDialect: XmlDialect = markup.xmlDialect

    var result: Xml.Element = content.xml

    result = WikiLink.embed(result, xmlDialect)
    result = Link.resolveInternalLinks(result, xmlDialect, content.source.page, content.source)

    // Process Footnotes (now that the links in their bodies are resolved)
    val footnoteNumbers: Map[String, Int] = Footnote
      .links(result, xmlDialect)
      .zipWithIndexFrom(1)
      .toMap

    val footnotes: Map[String, Footnote] = Footnote
      .bodies(result, xmlDialect)
      .map((correlationId, nodes) => Footnote(
        correlationId = correlationId,
        number = footnoteNumbers(correlationId),
        nodes = nodes
      ))
      .map(footnote => footnote.correlationId -> footnote)
      .toMap

    result = Footnote.removeBodies(result, markup)

    new PageContentResolved(
      content.source,
      result,
      content.toc,
      footnotes
    )
