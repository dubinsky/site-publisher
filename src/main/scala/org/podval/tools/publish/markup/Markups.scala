package org.podval.tools.publish.markup

import org.podval.tools.publish.processor.{Processor, Processors}
import zio.blocks.chunk.Chunk

// Known markup languages.
// Note: some XmlLike markups can have extensions other than `.xml`.
final class Markups:
  private var forExtension: Map[String, Markup] = Map.empty

  def forExtension(extension: String): Option[Markup] = forExtension.get(extension)

  private var forElement: Map[String, Markup] = Map.empty

  def forElement(element: String): Option[Markup] = forElement.get(element)

  private var all: Chunk[Markup] = Chunk.empty

  def add(
    markupKind: MarkupKind,
    processors: Seq[Processor],
    elements: Set[String] = Set.empty
  ): Unit =
    val markup: Markup = Markup(markupKind, Processors(processors))
    
    // TODO verify that extensions do not overlap
    val extensions: Set[String] = markupKind.extensions
    forExtension = forExtension ++ extensions.map(_ -> markup)

    // TODO verify that root elements do not overlap
    forElement = forElement ++ elements.map(_ -> markup)
