package org.podval.tools.publish.site

import org.podval.tools.publish.markup.Link
import org.podval.xml.Xml

final class LinkContext private(
  val url: String,
  val before: String,
  val element: String,
  val after: String
)

object LinkContext:
  // Note: Obsidian expands the context to the source level, which is good for searching - but doesn't look great
  // when there are non-wiki links in there;
  // I am going with just text, so the non-wiki links are not going to be visible...
  // Note: I can widen the context by going after grandparent etc. if it is too short - but Obsidian does not seem to do it...
  
  def apply(
    toFrom: Link,
    element: Xml.Element,
    before: Xml.Nodes,
    after: Xml.Nodes
  ): LinkContext = new LinkContext(
    url = toFrom.url,
    before = shortenContext(isBefore = true, contextText(before)),
    element = element.getText,
    after = shortenContext(isBefore = false, contextText(after))
  )

  /** Visible text only: skip `*-tip` subtrees (dates, footnotes, glossary, citations). */
  private def contextText(nodes: Xml.Nodes): String =
    nodes.map(nodeText).mkString

  private def nodeText(node: Xml.Node): String =
    node.asElement match
      case None => node.getText
      case Some(element) if isTip(element) => ""
      case Some(element) => contextText(element.getChildren)

  private def isTip(element: Xml.Element): Boolean =
    classTokens(element).exists(_.endsWith("-tip"))

  private def classTokens(element: Xml.Element): Seq[String] =
    element.getClasses ++
      element.get("tei-class").toSeq.flatMap(_.split(' ')).map(_.trim).filter(_.nonEmpty)
  
  private val contextLengthHalf: Int = 60

  private def shortenContext(isBefore: Boolean, string: String): String =
    if string.length <= contextLengthHalf then string else if isBefore then
      val result = string.substring(string.length - contextLengthHalf)
      val prefix = /*if result.startsWith(" ") then "" else*/ "..."
      prefix + result.trim
    else
      val result = string.substring(0, contextLengthHalf)
      val suffix = /*if result.endsWith(" ") then "" else*/ "..."
      result.trim + suffix

