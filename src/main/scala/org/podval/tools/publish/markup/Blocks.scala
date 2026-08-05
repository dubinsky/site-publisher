package org.podval.tools.publish.markup

import org.podval.xml.{HtmlClass, Xml, XmlDialect}
import org.podval.tools.publish.site.{PageError, PageErrorReporter}
import org.podval.tools.publish.util.Strings
import zio.blocks.chunk.Chunk

final class Blocks private(blocks: Seq[Block]):
  def resolve(id: String): Option[Link.ToBlock] = blocks.find(_.id == id).map(Link.ToBlock(_))

object Blocks:
  object BlockClass extends HtmlClass("wiki-block")

  // TODO according to the Obsidian documentation, block anchor can be added to a "structured block"
  // (e.g., a list) by putting it after the block, with empty lines before and after;
  // I'll deal with this later...
  def convertBlock(element: Xml.Element, errorReporter: PageErrorReporter): Option[Xml.Element] =
    val children: Chunk[Xml.Node] = element.getChildren
    if children.isEmpty then None else children.last.asText.flatMap: text =>
      val (before: String, id: Option[String]) = Strings.split(text, '^')
      id.flatMap: id =>
        if before.nonEmpty && !Character.isWhitespace(before.last) then None else Some:
          val result: Xml.Element = element.setChildren(
            children.init ++ Option.when(before.nonEmpty)(Xml.text(before)).toSeq
          )
          result.getId match
            case Some(idExisting) =>
              errorReporter.error(
                kind = PageError.NoId,
                message = s"Block id '$id' conflicts with existing id '$idExisting'"
              )
              result
            case None =>
              result.add(BlockClass).setId(id)

  def apply(
    xml: Xml.Element,
    xmlDialect: XmlDialect,
    errorReporter: PageErrorReporter
  ): Blocks =
    val result: Seq[Block] = xmlDialect.gather(xml, element =>
      if !element.has(BlockClass) then None else element
        .getId
        .map(Block(_))
        .orElse:
          errorReporter.error(PageError.NoId, s"Defect: No id on block $element")
          None
    )
    new Blocks(result)
