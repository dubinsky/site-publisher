package org.podval.tools.publish.processor

import org.podval.tools.publish.page.PageContent
import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.Xml
import zio.blocks.chunk.Chunk
import scala.annotation.tailrec

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
    content: PageContent,
    ids: IdGenerator,
    footnoteCorrelationIds: IdGenerator
  ): Option[Xml.Element]  = convert(
    element,
    content
  )

  protected def convert(
    element: Xml.Element,
    content: PageContent
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

  // Replace the elements satisfying the predicate with their children (repeatedly).
  final protected def unfold(element: Xml.Element, predicate: Xml.Element => Boolean): Xml.Element =
    @tailrec
    def unfold(children: Xml.Nodes): Xml.Nodes =
      var unfolded: Boolean = false
      val result = children.flatMap: child =>
        child.asElement match
          case Some(element) if predicate(element) =>
            unfolded = true
            element.getChildren
          case _ =>
            Chunk(child)

      if !unfolded then result else unfold(result)

    element.setChildren(unfold(element.getChildren))


