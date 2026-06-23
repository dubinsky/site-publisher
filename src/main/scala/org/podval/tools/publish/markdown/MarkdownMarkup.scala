package org.podval.tools.publish.markdown

import org.podval.tools.publish.markup.HtmlLikeMarkup
import org.podval.tools.publish.processor.SingleProcessor

final class MarkdownMarkup(
  processors: Seq[SingleProcessor]
) extends HtmlLikeMarkup(
  processors,
  Some("Markdown")
):
  // Wrap Markdown rendered as HTML in a 'div'.
  override def xmlContent(content: String): String = s"<div>${FlexMark.parseAndRenderMarkdown(content)}</div>"
