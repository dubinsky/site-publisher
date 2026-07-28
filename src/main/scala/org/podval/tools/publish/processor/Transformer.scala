package org.podval.tools.publish.processor

import org.podval.tools.publish.page.PageSource
import org.podval.xml.Xml

object Transformer:
  enum Stage:
    case General
    // Converter that converts links needs to run after everything that was to become a link had.
    case Footnotes

// Transforms XML as a whole.
abstract class Transformer(
  val stage: Transformer.Stage
) extends Processor:
  def transform(
    element: Xml.Element,
    source: PageSource
  ): Xml.Element
