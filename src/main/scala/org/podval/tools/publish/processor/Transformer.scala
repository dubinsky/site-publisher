package org.podval.tools.publish.processor

import org.podval.tools.publish.page.PageContent
import org.podval.xml.Xml

// Transforms XML as a whole.
abstract class Transformer(
  // Transformer that transforms footnotes needs to run after everything that was to become a footnote had.
  val transformsFootnotes: Boolean
) extends SingleProcessor:
  def transform(
    element: Xml.Element,
    content: PageContent
  ): Xml.Element
