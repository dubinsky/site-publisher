package org.podval.tools.publish.processor

import org.podval.tools.publish.page.PageSource
import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.Xml

// Converts individual XML elements.
trait Converter extends Processor:
  def convert(
    element: Xml.Element,
    source: PageSource,
    ids: IdGenerator,
    footnoteCorrelationIds: IdGenerator
  ): Xml.Element

  final protected def convertText(
    element: Xml.Element,
    converter: String => Seq[Xml.Node]
  ): Xml.Element =
    element.setChildren(element.getChildren.flatMap(xml => xml.asText.fold(Seq(xml))(converter)))

