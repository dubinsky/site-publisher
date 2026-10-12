package org.podval.tools.publish.markup

import org.podval.xml.Xml

object PlainText:
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
