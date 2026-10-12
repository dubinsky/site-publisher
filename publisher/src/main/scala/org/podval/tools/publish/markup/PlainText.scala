package org.podval.tools.publish.markup

import org.podval.xml.Xml

object PlainText:
  private val diplomaticSkip: Set[String] = Set(
    "note", "corr", "reg", "expan", "ex", "supplied", "rdg", "figDesc", "figure", "graphic"
  )

  private val diplomaticBlocks: Set[String] = Set(
    "p", "ab", "l", "lg", "head", "item", "cell", "row", "div", "quote", "table", "sp", "speaker"
  )

  /** Diplomatic `text/body` only. Not the tree after `TeiMarkup.process`. */
  def diplomaticBody(root: Xml.Element): String =
    val body: Option[Xml.Element] =
      root.childElements.find(_.isNamed("text")).flatMap(_.childElements.find(_.isNamed("body")))
    val raw: String = body.fold("")(element => walkDiplomatic(element).result())
    raw.replaceAll("\\s+", " ").trim

  private def walkDiplomatic(element: Xml.Element): StringBuilder =
    element.getChildren.foldLeft(new StringBuilder)(appendDiplomatic)

  private def appendDiplomatic(buf: StringBuilder, node: Xml.Node): StringBuilder =
    node.asElement match
      case Some(child) => appendDiplomaticElement(buf, child)
      case None => buf.append(node.asAtom.getOrElse(""))

  private def appendDiplomaticElement(buf: StringBuilder, element: Xml.Element): StringBuilder =
    val name: String = element.getName.localName
    if diplomaticSkip.contains(name) then buf
    else if name == "lb" then
      if element.get("break").exists(_.trim == "no") then buf else space(buf)
    else if name == "pb" then space(buf)
    else
      val spaced: StringBuilder = if diplomaticBlocks.contains(name) then space(buf) else buf
      element.getChildren.foldLeft(spaced)(appendDiplomatic)

  private def space(buf: StringBuilder): StringBuilder =
    if buf.isEmpty || buf.last.isWhitespace then buf else buf.append(' ')

  def visible(element: Xml.Element, skipNotes: Boolean, skipTips: Boolean): String =
    val buf: StringBuilder = element.getChildren.foldLeft(new StringBuilder): (acc, node) =>
      append(acc, node, skipNotes, skipTips)
    buf.result().replaceAll("\\s+", " ").trim

  private def append(
    buf: StringBuilder,
    node: Xml.Node,
    skipNotes: Boolean,
    skipTips: Boolean
  ): StringBuilder =
    node.asElement match
      case Some(child) if skipNotes && child.isNamed("note") => buf
      case Some(child) if skipTips && isTip(child) => buf
      case Some(child) =>
        child.getChildren.foldLeft(buf): (acc, nested) =>
          append(acc, nested, skipNotes, skipTips)
      case None => buf.append(node.asAtom.getOrElse(""))

  // Same token test as LinkContext.isTip. That method is private.
  private def isTip(element: Xml.Element): Boolean =
    val tokens: Seq[String] = element.getClasses ++
      element.get("tei-class").toSeq.flatMap(_.split(' ')).map(_.trim).filter(_.nonEmpty)
    tokens.exists(_.endsWith("-tip"))
