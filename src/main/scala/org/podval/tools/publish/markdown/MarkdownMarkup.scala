package org.podval.tools.publish.markdown

import org.podval.tools.publish.markup.HtmlLikeMarkup

object MarkdownMarkup extends HtmlLikeMarkup(
  name = "Markdown",
  extension = "md"
):
  // Wrap Markdown rendered as HTML in a 'div'.
  override def xmlContent(content: String): String =
    s"<div>${FlexMark.parseAndRenderMarkdown(content)}</div>"
