package org.podval.tools.publish.markup

import org.podval.tools.publish.site.PageErrorReporter
import org.podval.tools.publish.util.Strings
import org.podval.xml.Xml

object MarkdownWikiBlock:
  // Paragraph: `text ^id` on the same block. Lists, tables, quotes, and code fences:
  // `^id` on its own line after the block, with a blank line before it
  // (https://obsidian.md/help/syntax, Internal links / block embeds).
  def convert(element: Xml.Element, errorReporter: PageErrorReporter): Option[Xml.Element] =
    val (hoisted: Xml.Element, hoistedAny: Boolean) = hoistStandaloneIds(element, errorReporter)
    convertTrailing(hoisted, errorReporter).orElse(Option.when(hoistedAny)(hoisted))

  private def convertTrailing(element: Xml.Element, errorReporter: PageErrorReporter): Option[Xml.Element] =
    val children: Xml.Nodes = element.getChildren
    if children.isEmpty then None else children.last.asText.flatMap: text =>
      blockIdIn(text).map: (before, id) =>
        val stripped: Xml.Element = element.setChildren(
          children.init ++ Option.when(before.nonEmpty)(Xml.text(before)).toSeq
        )
        WikiBlock.mark(stripped, id, errorReporter)

  // Obsidian: after a list/table/quote/code block, `^id` is a following paragraph.
  private def hoistStandaloneIds(
    element: Xml.Element,
    errorReporter: PageErrorReporter
  ): (Xml.Element, Boolean) =
    val children: Xml.Nodes = element.getChildren.toList
    if children.size < 2 then (element, false) else
      var acc: Xml.Nodes = Nil
      var changed: Boolean = false
      for node <- children do
        node.asElement.flatMap(standaloneBlockId) match
          case Some(id) =>
            acc.lastIndexWhere(_.asElement.exists(isStructuredBlock)) match
              case -1 => acc = acc :+ node
              case i =>
                acc = acc.updated(i, WikiBlock.mark(acc(i).asElement.get, id, errorReporter))
                changed = true
          case None =>
            acc = acc :+ node
      if !changed then (element, false) else (element.setChildren(acc), true)

  private def standaloneBlockId(element: Xml.Element): Option[String] =
    if element.getName != "p" || element.getChildren.exists(_.asElement.isDefined) then None
    else blockIdIn(element.getText.trim).collect:
      case (before, id) if before.isEmpty => id

  private def blockIdIn(text: String): Option[(String, String)] =
    val (before: String, id: Option[String]) = Strings.split(text, '^')
    id.map(_.trim).filter(_.nonEmpty).flatMap: id =>
      Option.unless(before.nonEmpty && !Character.isWhitespace(before.last))((before, id))

  private def isStructuredBlock(element: Xml.Element): Boolean =
    element.getName match
      case "ul" | "ol" | "dl" | "table" | "pre" | "blockquote" | "p" => true
      case _ => false
