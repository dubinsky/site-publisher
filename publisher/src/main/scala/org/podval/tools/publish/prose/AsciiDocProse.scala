package org.podval.tools.publish.prose

import org.asciidoctor.ast.{Block, DescriptionList, Document, ListItem, StructuralNode}
import org.asciidoctor.log.LogRecord
import org.asciidoctor.{Asciidoctor, Options, SafeMode}
import scala.jdk.CollectionConverters.*

/** Reflow AsciiDoc by matching parsed lines back onto the original file. */
object AsciiDocProse extends ProseFormatter:
  override def kind: ProseKind = ProseKind.AsciiDoc
  private final case class Edit(start: Int, end: Int, text: String)

  private val logLines = new java.util.ArrayList[String]

  private lazy val asciidoctor: Asciidoctor =
    val created: Asciidoctor = Asciidoctor.Factory.create()
    created.registerLogHandler((record: LogRecord) => logLines.add(record.getMessage))
    created

  private lazy val loadOptions: Options =
    Options.builder().safe(SafeMode.SECURE).sourcemap(true).toFile(false).build()

  override def format(source: String, options: ProseOptions): Either[ProseFailure, String] =
    if source.contains('\r') then Left(ProseFailure.Crlf)
    else FrontMatterPeel.prose(source).flatMap: (prefix, body) =>
      asciidoctor.synchronized:
        logLines.clear()
        val document: Document = asciidoctor.load(escapeDirectives(body), loadOptions)
        val unterminated: Boolean = logLines.asScala.exists(_.toLowerCase.contains("unterminated"))
        if unterminated then Left(ProseFailure.Malformed("unterminated"))
        else
          val edits: List[Edit] = walk(document, body, options)
          Right(prefix + splice(body, edits))

  /** `context` plus parsed lines, in preorder, for the stability test. */
  def structure(source: String): Either[ProseFailure, List[String]] =
    if source.contains('\r') then Left(ProseFailure.Crlf)
    else FrontMatterPeel.prose(source).map: (_, body) =>
      asciidoctor.synchronized:
        logLines.clear()
        val document: Document = asciidoctor.load(escapeDirectives(body), loadOptions)
        snapshot(document)

  private def snapshot(node: StructuralNode): List[String] =
    val lines: String = parsedLines(node).mkString("\n")
    val here: String = context(node) + ":" + lines
    here :: childNodes(node).flatMap(snapshot)

  private val directiveLine = """^(?:include|ifdef|ifndef|ifeval|endif)::.*""".r
  private val hardBreakLine = """.* \+$""".r

  private def escapeDirectives(body: String): String =
    val (lines, trailing) = splitLines(body)
    val escaped: List[String] = lines.map: line =>
      if line.startsWith("\\") then line
      else if directiveLine.matches(line) then "\\" + line
      else line
    escaped.mkString("\n") + trailing

  private def walk(node: StructuralNode, body: String, options: ProseOptions): List[Edit] =
    context(node) match
      case "dlist" =>
        node.asInstanceOf[DescriptionList].getItems.asScala.toList.flatMap: entry =>
          val description: ListItem = entry.getDescription
          reflow(description, body, options).toList ++ childNodes(description).flatMap(walk(_, body, options))
      case "ulist" | "olist" | "colist" =>
        childNodes(node).flatMap: item =>
          reflow(item, body, options).toList ++ childNodes(item).flatMap(walk(_, body, options))
      case "paragraph" =>
        reflow(node, body, options).toList
      case "admonition" if childNodes(node).isEmpty =>
        reflow(node, body, options).toList
      case "admonition" =>
        childNodes(node).flatMap(walk(_, body, options))
      case "verse" | "listing" | "literal" | "stem" | "pass" | "table" | "image" | "audio" | "video" |
           "toc" | "thematic_break" | "page_break" | "floating_title" =>
        Nil
      case _ =>
        childNodes(node).flatMap(walk(_, body, options))

  private def reflow(node: StructuralNode, body: String, options: ProseOptions): Option[Edit] =
    val parsed: List[String] = parsedLines(node).filter(_.nonEmpty)
    val location = node.getSourceLocation
    if parsed.isEmpty || location == null || parsed.exists(directiveLine.matches) || hardbreaks(node) then None
    else
      val starts: Vector[Int] = lineStarts(body)
      matchRun(body, starts, parsed, location.getLineNumber).flatMap: (start, end, contentColumn) =>
        val fileLines: List[String] = matchedFileLines(body, starts, location.getLineNumber, parsed.length)
        val tableRow: Boolean = fileLines.exists(_.trim.startsWith("|")) || parsed.exists(_.trim.startsWith("|"))
        if fileLines.exists(hardBreakLine.matches) || tableRow then None
        else
          val joined: String = joinParsed(parsed)
          val tokens = AsciiDocScanner.tokens(joined)
          val laid: String = ProseLayout.layout(
            tokens, options, contentColumn, contPrefix(node, contentColumn), AsciiDocLeadIn.matches
          )
          val text: String = if end > start && body.charAt(end - 1) == '\n' then laid + "\n" else laid
          Some(Edit(start, end, text))

  private def joinParsed(lines: List[String]): String = lines match
    case Nil => ""
    case head :: tail =>
      tail.foldLeft(head): (acc, line) =>
        acc + joinSeparator(acc, line) + line

  /** A newline inside an open `#` span is not a space when both sides are Hebrew.
    * The span was broken through a word. A real space between words stays a space,
    * and a `#` span is one atom, so wrapping does not turn that space into a newline.
    */
  private def joinSeparator(left: String, right: String): String =
    if hebrewSplit(left, right) then "" else " "

  private def hebrewSplit(left: String, right: String): Boolean =
    insideHash(left) && lastCode(left).exists(hebrewChar) && firstCode(right).exists(hebrewChar)

  private def lastCode(text: String): Option[Int] =
    if text.isEmpty then None else Some(text.codePointBefore(text.length))

  private def firstCode(text: String): Option[Int] =
    if text.isEmpty then None else Some(text.codePointAt(0))

  private def hebrewChar(codePoint: Int): Boolean =
    (codePoint >= 0x05d0 && codePoint <= 0x05ea) ||
      (codePoint >= 0x0591 && codePoint <= 0x05c7 && Character.getType(codePoint) == Character.NON_SPACING_MARK)

  /** `closer` is `#` or `##` while that span is open. `\#` is literal. */
  private def insideHash(text: String): Boolean =
    def rec(rest: String, closer: String): Boolean =
      if rest.isEmpty then closer.nonEmpty
      else if rest.startsWith("\\") && rest.length > 1 then
        val size: Int = 1 + Character.charCount(rest.codePointAt(1))
        rec(rest.substring(size), closer)
      else if closer.isEmpty && rest.startsWith("##") then rec(rest.substring(2), "##")
      else if closer.isEmpty && rest.startsWith("#") then rec(rest.substring(1), "#")
      else if closer.nonEmpty && rest.startsWith(closer) then rec(rest.substring(closer.length), "")
      else
        val size: Int = Character.charCount(rest.codePointAt(0))
        rec(rest.substring(size), closer)
    rec(text, "")

  private def hardbreaks(node: StructuralNode): Boolean =
    node.hasAttribute("hardbreaks-option") || node.hasAttribute("options") &&
      String.valueOf(node.getAttribute("options")).contains("hardbreaks")

  private def contPrefix(node: StructuralNode, contentColumn: Int): String =
    if contentColumn > 0 then " " * contentColumn
    else if quoteLike(node.getParent) then "  "
    else ""

  private def quoteLike(node: org.asciidoctor.ast.ContentNode): Boolean =
    node match
      case null => false
      case structural: StructuralNode =>
        val kind: String = context(structural)
        kind == "quote" || kind == "sidebar" || kind == "example"
      case _ => false

  private def parsedLines(node: StructuralNode): List[String] = node match
    case item: ListItem =>
      val source: String = Option(item.getSource).getOrElse("")
      if source.isEmpty then Nil else splitLines(source)._1
    case block: Block =>
      block.getLines.asScala.toList
    case _ =>
      Nil

  private def matchRun(
    body: String,
    starts: Vector[Int],
    parsed: List[String],
    lineNo: Int
  ): Option[(Int, Int, Int)] =
    val first: Int = lineNo - 1
    if first < 0 || first >= starts.length || first + parsed.length > starts.length then None
    else
      val stripped: String = stripTrailing(lineText(body, starts, first))
      val head: String = parsed.head
      if !stripped.endsWith(head) then None
      else
        val prefix: String = stripped.substring(0, stripped.length - head.length)
        val contentColumn: Int = ProseLayout.columns(prefix)
        val restOk: Boolean = parsed.tail.zipWithIndex.forall: (line, offset) =>
          val fileLine: String = stripTrailing(lineText(body, starts, first + 1 + offset))
          fileLine == line || dropIndent(fileLine, contentColumn) == line || fileLine.dropWhile(_ == ' ') == line
        if !restOk then None
        else
          val start: Int = starts(first) + prefix.length
          val last: Int = first + parsed.length - 1
          Some((start, lineEnd(body, starts, last), contentColumn))

  private def matchedFileLines(body: String, starts: Vector[Int], lineNo: Int, count: Int): List[String] =
    (0 until count).toList.map(offset => lineText(body, starts, lineNo - 1 + offset))

  private def dropIndent(line: String, columns: Int): String =
    val (spaces, _) = line.span(_ == ' ')
    line.substring(math.min(columns, spaces.length))

  private def stripTrailing(line: String): String =
    val (_, bodyRev) = line.reverse.span(ch => ch == ' ' || ch == '\t')
    bodyRev.reverse

  private def lineStarts(body: String): Vector[Int] =
    0 +: Iterator.unfold(0): from =>
      val at: Int = body.indexOf('\n', from)
      if at < 0 || at + 1 > body.length then None else Some((at + 1, at + 1))
    .toVector

  private def lineText(body: String, starts: Vector[Int], index: Int): String =
    val from: Int = starts(index)
    val to: Int = if index + 1 < starts.length then starts(index + 1) else body.length
    val slice: String = body.substring(from, to)
    if slice.endsWith("\n") then slice.dropRight(1) else slice

  private def lineEnd(body: String, starts: Vector[Int], index: Int): Int =
    if index + 1 < starts.length then starts(index + 1) else body.length

  private def splitLines(text: String): (List[String], String) =
    if text.endsWith("\n") then (text.dropRight(1).split("\n", -1).toList, "\n")
    else (text.split("\n", -1).toList, "")

  private def childNodes(node: StructuralNode): List[StructuralNode] =
    node.getBlocks.asScala.toList

  private def context(node: StructuralNode): String =
    val raw: String = String.valueOf(node.getContext)
    if raw.startsWith(":") then raw.drop(1) else raw

  private def splice(source: String, edits: List[Edit]): String =
    val sorted: List[Edit] = edits.sortBy(_.start)
    val out = new StringBuilder
    var cursor: Int = 0
    sorted.foreach: edit =>
      if edit.start >= cursor then
        out.append(source.substring(cursor, edit.start))
        out.append(edit.text)
        cursor = edit.end
    out.append(source.substring(cursor))
    out.result()
