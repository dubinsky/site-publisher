package org.podval.tools.publish.processor

import org.podval.tools.publish.page.PageContent
import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.Xml

// Converts individual XML elements.
abstract class ConverterWithIds(
  // Converter that converts links needs to run after everything that was to become a link had.
  val convertLinks: Boolean = false
) extends SingleProcessor:

  def convertWithIds(
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

  final protected def renameElement(
    name: String,
    element: Xml.Element
  ): Xml.Element = element
    .addClass(element.getName)
    .rename(name)

  final protected def copyAttribute(
    from: String,
    to: String,
    element: Xml.Element
  ): Xml.Element =
    element.get(from).fold(element)(element.set(to, _))


