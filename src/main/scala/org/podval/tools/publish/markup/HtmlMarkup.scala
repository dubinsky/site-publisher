package org.podval.tools.publish.markup

import org.podval.tools.publish.processor.SingleProcessor

final class HtmlMarkup(processors: Seq[SingleProcessor]) extends HtmlLikeMarkup(processors) with XmlParsableMarkup

object HtmlMarkup:
  val extension: String = "html"
