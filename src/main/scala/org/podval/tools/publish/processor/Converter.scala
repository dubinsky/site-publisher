package org.podval.tools.publish.processor

import org.podval.tools.publish.page.PageSource
import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.Xml

object Converter:
  enum Stage:
    case General
    // Converter that converts links needs to run after everything that was to become a link had.
    case Links

// Converts individual XML elements.
abstract class Converter extends Processor:
  def stage: Converter.Stage = Converter.Stage.General

  def convert(
    element: Xml.Element,
    source: PageSource,
    ids: IdGenerator,
    footnoteCorrelationIds: IdGenerator
  ): Option[Xml.Element]  = convert(
    element,
    source
  )

  protected def convert(
    element: Xml.Element,
    source: PageSource,
  ): Option[Xml.Element]  = convert(
    element
  )

  protected def convert(
    element: Xml.Element
  ): Option[Xml.Element] =
    None

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

