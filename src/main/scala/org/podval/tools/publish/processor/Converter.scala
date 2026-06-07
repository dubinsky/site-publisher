package org.podval.tools.publish.processor

import org.podval.tools.publish.page.PageContent
import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.Xml

// Converts individual XML elements.
abstract class Converter(
  // Converter that converts links needs to run after everything that was to become a link had.
  val convertLinks: Boolean = false
) extends SingleProcessor:
  def convert(
    element: Xml.Element,
    content: PageContent,
    ids: IdGenerator,
    footnoteCorrelationIds: IdGenerator
  ): Xml.Element

  final protected def convertText(
    element: Xml.Element,
    converter: String => Seq[Xml.Node]
  ): Xml.Element =
    element.setChildren(element.getChildren.flatMap(xml => xml.asText.fold(Seq(xml))(converter)))

