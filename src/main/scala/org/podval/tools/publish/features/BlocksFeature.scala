package org.podval.tools.publish.features

import org.podval.tools.publish.util.Strings
import org.podval.tools.publish.PageError
import org.podval.xml.{HtmlClass, Xml, XmlAttribute}
import zio.blocks.chunk.Chunk

// TODO according to the Obsidian documentation, block anchor can be added to a "structured block"
// (e.g., a list) by putting it after the block, with empty lines before and after;
// I'll deal with this later...
object BlocksFeature extends Feature:
  object BlockClass extends HtmlClass("wiki-block")

  override def process(
    element: Xml.Element,
    context: Feature.ProcessContext
  ): Xml.Element =
    val children: Chunk[Xml.Node] = element.getChildren
    if children.isEmpty then element else children.last.asText.fold(element): text =>
      val (before: String, id: Option[String]) = Strings.split(text, '^')
      id.fold(element): id =>
        if before.nonEmpty && !Character.isWhitespace(before.last) then element else
          val result: Xml.Element = element.setChildren(
            children.init ++ Option.when(before.nonEmpty)(Xml.text(before)).toSeq
          )
          result.get(XmlAttribute.Id) match
            case Some(idExisting) =>
              context.errorReporter.error(PageError.NoId, s"Block id '$id' conflicts with existing id '$idExisting'", result)
            case None =>
              result.set(XmlAttribute.Id, id).add(BlockClass)
