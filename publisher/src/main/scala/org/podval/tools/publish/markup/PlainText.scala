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
    val raw: String = body.fold("")(element => new Diplomatic(element).result())
    raw.replaceAll("\\s+", " ").trim

  // U+002D only. A maqaf (U+05BE) is not a line-break hyphen.
  private val breakHyphen: Char = '-'

  private def isLetterOrNumber(c: Char): Boolean =
    if c.isLetter then true
    else
      val kind: Int = Character.getType(c)
      kind == Character.DECIMAL_DIGIT_NUMBER ||
        kind == Character.LETTER_NUMBER ||
        kind == Character.OTHER_NUMBER

  // Joined spelling for a break="no" hyphen. The diplomatic characters stay.
  // The extra token is the next word after that word, so the snippet still sees the hyphen.
  private final class Diplomatic(root: Xml.Element):
    private val buf: StringBuilder = new StringBuilder
    private val joined: StringBuilder = new StringBuilder
    private val held: StringBuilder = new StringBuilder
    private var joining: Boolean = false
    private var closed: Boolean = false
    private var skipIntroducedWs: Boolean = false
    private var hyphenPending: Boolean = false

    def result(): String =
      root.getChildren.foreach(appendNode)
      endWord()
      buf.result()

    private def appendNode(node: Xml.Node): Unit =
      node.asElement match
        case Some(child) => appendElement(child)
        case None => appendAtom(node.asAtom.getOrElse(""))

    private def appendElement(element: Xml.Element): Unit =
      val name: String = element.getName.localName
      if diplomaticSkip.contains(name) then ()
      else if name == "lb" && isBreakNo(element) then onBreakNo()
      else if name == "lb" || name == "pb" then
        endWord()
        space()
      else if diplomaticBlocks.contains(name) then
        endWord()
        space()
        element.getChildren.foreach(appendNode)
      else
        continueThroughInline()
        element.getChildren.foreach(appendNode)

    private def isBreakNo(element: Xml.Element): Boolean =
      element.get("break").exists(_.trim == "no")

    private def onBreakNo(): Unit =
      if joining && hyphenPending then
        buf.append(held)
        held.clear()
        hyphenPending = false
        skipIntroducedWs = true
      else if !joining && breakHyphenAtEnd then
        joined.clear()
        joined.append(runBeforeBreakHyphen)
        joining = true
        closed = false
        skipIntroducedWs = true
        hyphenPending = false
        held.clear()

    private def breakHyphenAtEnd: Boolean =
      buf.toString.reverse.dropWhile(_.isWhitespace).startsWith(breakHyphen.toString)

    private def runBeforeBreakHyphen: String =
      buf.toString.reverse.dropWhile(_.isWhitespace).drop(1).takeWhile(isLetterOrNumber).reverse

    private def continueThroughInline(): Unit =
      if joining && hyphenPending then
        if held.nonEmpty then endWord()
        else
          hyphenPending = false
          closed = true

    private def endWord(): Unit =
      val ws: String = if hyphenPending then held.toString else ""
      held.clear()
      hyphenPending = false
      flushJoined()
      if ws.nonEmpty then buf.append(ws)

    private def flushJoined(): Unit =
      if joining && joined.nonEmpty then
        space()
        buf.append(joined.toString)
      joining = false
      closed = false
      skipIntroducedWs = false
      joined.clear()

    private def space(): Unit =
      if buf.nonEmpty && !buf.last.isWhitespace then buf.append(' ')

    private def appendAtom(text: String): Unit =
      if text.isEmpty then ()
      else if !joining then buf.append(text)
      else appendJoining(text)

    private def appendJoining(text: String): Unit =
      if text.isEmpty then ()
      else if !joining then buf.append(text)
      else if skipIntroducedWs then
        val (ws, rest) = text.span(_.isWhitespace)
        buf.append(ws)
        if rest.nonEmpty then
          skipIntroducedWs = false
          appendJoining(rest)
      else if hyphenPending then
        val (ws, rest) = text.span(_.isWhitespace)
        if rest.isEmpty then held.append(ws)
        else if ws.nonEmpty || held.nonEmpty then
          val boundary: String = held.toString + ws
          held.clear()
          hyphenPending = false
          flushJoined()
          buf.append(boundary)
          appendJoining(rest)
        else
          hyphenPending = false
          closed = true
          appendJoining(text)
      else if closed then
        val (ws, afterWs) = text.span(_.isWhitespace)
        if ws.nonEmpty then
          flushJoined()
          buf.append(ws)
          appendJoining(afterWs)
        else
          val (word, after) = text.span(!_.isWhitespace)
          buf.append(word)
          appendJoining(after)
      else
        val (run, rest) = text.span(isLetterOrNumber)
        if run.nonEmpty then
          buf.append(run)
          joined.append(run)
          appendJoining(rest)
        else
          val (ws, afterWs) = text.span(_.isWhitespace)
          if ws.nonEmpty then
            flushJoined()
            buf.append(ws)
            appendJoining(afterWs)
          else if text.head == breakHyphen then
            buf.append(breakHyphen)
            hyphenPending = true
            appendJoining(text.drop(1))
          else
            val (punct, after) = text.span: c =>
              !c.isWhitespace && c != breakHyphen && !isLetterOrNumber(c)
            buf.append(punct)
            closed = true
            appendJoining(after)

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
