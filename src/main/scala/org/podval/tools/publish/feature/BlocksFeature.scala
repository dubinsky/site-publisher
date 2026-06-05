package org.podval.tools.publish.feature

import org.podval.tools.publish.util.{IdGenerator, Strings}
import org.podval.tools.publish.PageError
import org.podval.tools.publish.page.PageSource
import org.podval.tools.publish.processor.{Converter, Feature}
import org.podval.xml.Xml
import zio.blocks.chunk.Chunk

// TODO according to the Obsidian documentation, block anchor can be added to a "structured block"
// (e.g., a list) by putting it after the block, with empty lines before and after;
// I'll deal with this later...
final class BlocksFeature extends Feature(
  converter = Some(BlocksFeature.BlocksConverter())
)

object BlocksFeature:
  private final class BlocksConverter extends Converter: 
    override def convert(
      element: Xml.Element,
      source: PageSource,
      ids: IdGenerator,
      footnoteCorrelationIds: IdGenerator
    ): Xml.Element =
      val children: Chunk[Xml.Node] = element.getChildren
      if children.isEmpty then element else children.last.asText.fold(element): text =>
        val (before: String, id: Option[String]) = Strings.split(text, '^')
        id.fold(element): id =>
          if before.nonEmpty && !Character.isWhitespace(before.last) then element else
            val result: Xml.Element = element.setChildren(
              children.init ++ Option.when(before.nonEmpty)(Xml.text(before)).toSeq
            )
            result.getId match
              case Some(idExisting) =>
                source.errorReporter.error(
                  PageError.NoId,
                  s"Block id '$id' conflicts with existing id '$idExisting'",
                  result
                )
              case None =>
                Links.markBlock(result).setId(id)
