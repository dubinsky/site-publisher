package org.podval.tools.publish.markdown

import org.podval.tools.publish.markup.{HtmlMarkup, Markup}
import org.podval.tools.publish.page.PageSource
import org.podval.xml.{Html, HtmlXmlDialect, Xml}
import java.io.File

object MarkdownMarkup extends Markup(
  name = "Markdown",
  allowsInternalFrontMatter = true,
  extension = "md",
  rendersToXml = true,
  xmlDialect = HtmlXmlDialect,
):
  override def postProcess(source: PageSource, xml: Xml.Element): Xml.Element =
    source.xmlDialect.transform(xml, element =>
      WikiLinks.embedWikiLink(element).getOrElse(element)
    )

  override def isSpuriousFootnotesDiv(element: Xml.Element): Boolean =
    element.getName == "div" && element.hasClass("footnotes")

  override def xmlContent(content: String, sourceFile: File): String =
    // Wrap Markdown rendered as HTML in a 'div'.
    s"<div>${FlexMark.parseAndRenderMarkdown(content)}</div>"

  override def process(
    source: PageSource,
    xml: Xml.Element
  ): (Xml.Element, Option[Xml.Element]) =
    val result: Xml.Element = xmlDialect.transform(xml, (element: Xml.Element) =>
      var result: Xml.Element = element
      result = Blocks.markBlock(result, source).getOrElse(result)
      result = WikiLinks.convertWikiLinks(result).getOrElse(result)
      result = MarkdownFootnotes.convertFootnotes(result).getOrElse(result)
      result = FlexMarkFootnotes.convertFootnoteLink(result).getOrElse(result)
      result = FlexMarkFootnotes.convertFootnoteBody(result).getOrElse(result)
      result
    )
    HtmlMarkup.process(
      source,
      result
    )

  // Kramdown Toc Marker
  override def isTocPlaceholder(element: Html.Element): Boolean =
    element.getName == "ul" && element.getChildren.exists: node =>
      node.asElement.fold(false): child =>
        child.getName == "li" &&
        child.getChildren.length == 1 &&
        child.getChildren.head.asText.fold(false): text =>
          text.endsWith("{:toc}")
