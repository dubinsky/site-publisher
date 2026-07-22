package org.podval.tools.publish.markdown

import org.podval.tools.publish.{Path, Site}
import org.podval.tools.publish.markup.HtmlLikeMarkup

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
