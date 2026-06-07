package org.podval.tools.publish.processor

import org.podval.tools.publish.page.PageContent
import org.podval.xml.Xml

// Transforms XML as a whole.
trait Transformer extends Processor:
  def transform(
    element: Xml.Element,
    content: PageContent
  ): Xml.Element
