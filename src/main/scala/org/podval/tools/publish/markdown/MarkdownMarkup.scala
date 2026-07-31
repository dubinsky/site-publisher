package org.podval.tools.publish.markdown

import org.podval.tools.publish.html.HtmlMarkup
import org.podval.tools.publish.markup.{Converter, Markup, Processor}
import org.podval.tools.publish.page.PageSource
import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.{Html, HtmlXmlDialect, Xml}
import java.io.File

object MarkdownMarkup extends Markup(
  name = "Markdown",
  allowsInternalFrontMatter = true,
  extension = "md",
  rendersToXml = true,
  xmlDialect = HtmlXmlDialect,
):
  override def process(
    source: PageSource,
    ids: IdGenerator,
    xml: Xml.Element
  ): (Xml.Element, Option[Xml.Element]) = HtmlMarkup.process(
    source,
    ids,
    xml,
    Seq(
      BlocksConverter(source),
      WikiLinksConverter(),
      MarkdownFootnotesConverter(),
      FlexMarkFootnoteLinksConverter(),
      FlexMarkFootnoteBodiesConverter(),
    )
  )

  override def postProcessors(
    source: PageSource
  ): Seq[Converter] = Seq(
    WikiLinksPostConverter()
  )

  override def isSpuriousFootnotesDiv(element: Xml.Element): Boolean =
    element.getName == "div" && element.hasClass("footnotes")

  override def xmlContent(content: String, sourceFile: File): String =
    // Wrap Markdown rendered as HTML in a 'div'.
    s"<div>${FlexMark.parseAndRenderMarkdown(content)}</div>"

  // Kramdown Toc Marker
  override def isTocPlaceholder(element: Html.Element): Boolean =
    element.getName == "ul" && element.getChildren.exists: node =>
      node.asElement.fold(false): child =>
        child.getName == "li" &&
        child.getChildren.length == 1 &&
        child.getChildren.head.asText.fold(false): text =>
          text.endsWith("{:toc}")
