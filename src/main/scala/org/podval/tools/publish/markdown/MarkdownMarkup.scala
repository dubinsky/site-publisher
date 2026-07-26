package org.podval.tools.publish.markdown

import org.podval.tools.publish.link.Fragment.Section
import org.podval.tools.publish.{Path, Site}
import org.podval.tools.publish.markup.{HtmlSections, MarkupKind}
import org.podval.tools.publish.page.PageContent
import org.podval.tools.publish.processor.Processor
import org.podval.xml.{Html, HtmlXmlDialect, Xml}

object MarkdownMarkup extends MarkupKind(
  name = "Markdown",
  allowsInternalFrontMatter = true,
  extension = "md",
  rendersToXml = true,
  xmlDialect = HtmlXmlDialect,
):
  override def pageHeader(content: PageContent): Html.Element = MarkupKind.pageHeader(content)

  override def sections(content: PageContent): Seq[Section] = HtmlSections.sections(content)

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
    new FlexMarkFootnoteBodiesConverter,
    new KramdownTocHtmlConverter
  )

  def isFootnotesDiv(element: Xml.Element): Boolean =
    element.getName == "div" && element.hasClass("footnotes")
