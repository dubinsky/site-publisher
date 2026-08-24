package org.podval.tools.publish.markup

import org.podval.xml.{HtmlClass, Xml}
import zio.blocks.chunk.Chunk

object DescriptionList:
  def groupItems(
    nodes: Xml.Nodes,
    itemClass: HtmlClass,
    takeTermId: Xml.Element => (Option[String], Xml.Element)
  ): Xml.Nodes =
    var result: List[Xml.Node] = Nil
    var group: List[Xml.Node] = Nil
    var groupId: Option[String] = None

    def flush(): Unit =
      if group.nonEmpty then
        result = result :+ Xml
          .element("div")
          .add(itemClass)
          .setId(groupId)
          .setChildren(Chunk.from(group))
        group = Nil
        groupId = None

    nodes.foreach: node =>
      node.asElement match
        case Some(element) if element.getName == "dt" =>
          flush()
          val (id, dt) = takeTermId(element)
          groupId = id
          group = List(dt)
        case Some(element) if element.getName == "dd" =>
          if group.isEmpty then result = result :+ element
          else group = group :+ element
        case Some(element) =>
          flush()
          result = result :+ element
        case None =>
          if !node.isWhitespace then
            flush()
            result = result :+ node

    flush()
    Chunk.from(result)

  def stripExplicitTermId(dt: Xml.Element): (Option[String], Xml.Element) =
    val (leading, rest) = dt.getChildren.span(node =>
      node.isWhitespace || node.asElement.exists(isEmptyIdAnchor)
    )
    val fromAnchor: Option[String] = leading.flatMap(_.asElement).flatMap(_.getId).headOption
    val stripped: Xml.Element = if fromAnchor.isEmpty then dt else dt.setChildren(rest)
    val id: Option[String] = fromAnchor.orElse(stripped.getId)
    val term: Xml.Element = if stripped.getId.isEmpty then stripped else stripped.setId("")
    (id, term)

  private def isEmptyIdAnchor(element: Xml.Element): Boolean =
    element.isA &&
    element.getId.nonEmpty &&
    element.getHref.isEmpty &&
    element.getChildren.forall(_.isWhitespace)
