package org.podval.tools.publish.markup

import org.podval.tools.publish.page.PageSource
import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.Xml

object Converter:
  val id: Converter = new Converter {}

  def concat(converters: Converter*): Converter = converters.reduce(_.andThen(_))

  private final class AndThen(left: Converter, right: Converter) extends Converter:
    override protected def convert(
      element: Xml.Element,
      source: PageSource,
      ids: IdGenerator,
      footnoteCorrelationIds: IdGenerator
    ): Option[Xml.Element] =
      val convertedByLeft: Xml.Element = left.doConvert(
        element,
        source,
        ids,
        footnoteCorrelationIds
      )

      val result: Xml.Element = right.doConvert(
        convertedByLeft,
        source,
        ids,
        footnoteCorrelationIds
      )
      
      Some(result)

// Converts individual XML elements.
abstract class Converter:
  def andThen(right: Converter): Converter = Converter.AndThen(this, right)

  final def doConvert(
    element: Xml.Element,
    source: PageSource,
    ids: IdGenerator,
    footnoteCorrelationIds: IdGenerator
  ): Xml.Element =
    convert(
      element,
      source,
      ids, 
      footnoteCorrelationIds
    )
      .getOrElse(element)
  
  protected def convert(
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

