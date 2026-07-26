package org.podval.tools.publish.markdown

import org.podval.tools.publish.{Path, Site}
import org.podval.tools.publish.markup.HtmlLikeMarkup
import org.podval.tools.publish.processor.Processor
import org.podval.xml.Xml

object MarkdownMarkup extends HtmlLikeMarkup(
  name = "Markdown",
  allowsInternalFrontMatter = true,
  extension = "md",
  rendersToXml = true
):
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
