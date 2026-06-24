package org.podval.tools.publish.markup

import org.podval.tools.publish.processor.Processor
import zio.blocks.chunk.Chunk

// Known markup languages; note: some XmlLike markups can have extensions other than `.xml`.
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
    val markup: Markup = Markup(markupKind, processors.flatMap(_.processors))
    
    // TODO verify that extensions do not overlap
    val extensions: Set[String] = markupKind.extensions
    forExtension = forExtension ++ extensions.map(_ -> markup)

    // TODO verify that root elements do not overlap
    forElement = forElement ++ elements.map(_ -> markup)
