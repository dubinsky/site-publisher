package org.podval.tools.publish.markdown

import org.podval.tools.publish.link.Fragment.Section
import org.podval.tools.publish.{Path, Site}
import org.podval.tools.publish.markup.{HtmlSections, MarkupKind, Processor}
import org.podval.tools.publish.page.{MarkupPage, PageSource}
import org.podval.xml.{Html, HtmlXmlDialect, Xml}

object MarkdownMarkup extends MarkupKind(
  name = "Markdown",
  allowsInternalFrontMatter = true,
  extension = "md",
  rendersToXml = true,
  xmlDialect = HtmlXmlDialect,
):
  override def pageHeader(page: MarkupPage): Html.Element = MarkupKind.pageHeader(page)

  override def sections(source: PageSource, xml: Xml.Element): Seq[Section] = HtmlSections.sections(source, xml)

  override def xmlContent(
    site: Site,
    sourcePath: Path,
    content: String
  ): String =
    // Wrap Markdown rendered as HTML in a 'div'.
    s"<div>${FlexMark.parseAndRenderMarkdown(content)}</div>"

  def processors: Seq[Processor] = Seq(
    new BlocksConverter,
    new WikiLinksConverter,
    new WikiLinksPostConverter,
    new MarkdownFootnotesConverter,
    new FlexMarkFootnoteLinksConverter,
    new FlexMarkFootnoteBodiesConverter
  )

  def isFootnotesDiv(element: Xml.Element): Boolean =
    element.getName == "div" && element.hasClass("footnotes")

  def isKramdownTocMarker(element: Html.Element): Boolean =
    element.getName == "ul" && element.getChildren.exists: node =>
      node.asElement.fold(false): child =>
        child.getName == "li" &&
        child.getChildren.length == 1 &&
        child.getChildren.head.asText.fold(false): text =>
          text.endsWith("{:toc}")
