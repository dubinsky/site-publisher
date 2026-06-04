package org.podval.tools.publish.processor

import org.podval.tools.publish.page.PageSource
import org.podval.xml.Xml

// Transforms XML as a whole.
trait Transformer extends Processor:
  def transform(
    element: Xml.Element,
    pageSource: PageSource
  ): Xml.Element
