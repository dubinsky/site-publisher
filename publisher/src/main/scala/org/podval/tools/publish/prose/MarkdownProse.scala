package org.podval.tools.publish.prose

import com.vladsch.flexmark.ast.*
import com.vladsch.flexmark.ext.definition.{DefinitionItem, DefinitionList, DefinitionTerm}
import com.vladsch.flexmark.ext.footnotes.{Footnote, FootnoteBlock}
import com.vladsch.flexmark.ext.gfm.strikethrough.Strikethrough
import com.vladsch.flexmark.ext.gfm.tasklist.TaskListItem
import com.vladsch.flexmark.ext.tables.TableBlock
import com.vladsch.flexmark.ext.wikilink.{WikiImage, WikiLink, WikiLinkExtension}
import com.vladsch.flexmark.parser.Parser
import com.vladsch.flexmark.util.ast.{Block, Document, Node}
import com.vladsch.flexmark.util.data.MutableDataSet
import org.podval.tools.publish.markup.MarkdownMarkup
import scala.jdk.CollectionConverters.SeqHasAsJava

import ProseLayout.Token

/** Reflow Markdown by splicing Flexmark offsets back into the original text. */
object MarkdownProse extends ProseFormatter:
  override def kind: ProseKind = ProseKind.Markdown
  private final case class Edit(start: Int, end: Int, text: String)

  override def format(source: String, options: ProseOptions): Either[ProseFailure, String] =
    if source.contains('\r') then Left(ProseFailure.Crlf)
    else FrontMatterPeel.prose(source).map: (prefix, body) =>
      val document: Node = parser.parse(body)
      val edits: List[Edit] = walk(document, body, options)
      prefix + splice(body, edits)

  /** Block class names in preorder, for the parse-stability test. */
  def blockPreorder(source: String): Either[ProseFailure, List[String]] =
    if source.contains('\r') then Left(ProseFailure.Crlf)
    else FrontMatterPeel.prose(source).map: (_, body) =>
      blocks(parser.parse(body))

  private def blocks(node: Node): List[String] =
    val here: List[String] = if node.isInstanceOf[Block] then List(node.getClass.getSimpleName) else Nil
    here ++ children(node).flatMap(blocks)

  private lazy val parser: Parser =
    val data = new MutableDataSet()
    data.set(WikiLinkExtension.LINK_FIRST_SYNTAX, java.lang.Boolean.TRUE)
    data.set(WikiLinkExtension.ALLOW_ANCHORS, java.lang.Boolean.TRUE)
    data.set(WikiLinkExtension.IMAGE_LINKS, java.lang.Boolean.TRUE)
    Parser.builder(data).extensions((MarkdownMarkup.proseParserExtensions :+ WikiLinkExtension.create).asJava).build

  private def walk(node: Node, source: String, options: ProseOptions): List[Edit] =
    node match
      case paragraph: Paragraph =>
        editParagraph(paragraph, source, options).toList
      case _: Heading | _: FencedCodeBlock | _: IndentedCodeBlock | _: ThematicBreak | _: HtmlBlock |
           _: HtmlCommentBlock | _: Reference | _: TableBlock | _: DefinitionTerm =>
        Nil
      case _ if descend(node) =>
        children(node).flatMap(walk(_, source, options))
      case _ =>
        Nil

  private def descend(node: Node): Boolean = node match
    case _: Document | _: BlockQuote | _: BulletList | _: OrderedList | _: ListItem | _: TaskListItem |
         _: DefinitionList | _: DefinitionItem | _: FootnoteBlock =>
      true
    case _ => false

  private def editParagraph(paragraph: Paragraph, source: String, options: ProseOptions): Option[Edit] =
    val start: Int = paragraph.getStartOffset
    val end: Int = paragraph.getEndOffset
    continuation(paragraph, source) match
      case None => None
      case Some(contPrefix) =>
        val lineStart: Int = startOfLine(source, start)
        val already: Int = ProseLayout.columns(source.substring(lineStart, start))
        val tokens: List[Token] = paragraphTokens(paragraph, source)
        val body: String = ProseLayout.layout(tokens, options, already, contPrefix, MarkdownLeadIn.matches)
        val text: String = if source.substring(start, end).endsWith("\n") then body + "\n" else body
        Some(Edit(start, end, text))

  private def continuation(paragraph: Node, source: String): Option[String] =
    val start: Int = paragraph.getStartOffset
    val observed: String = source.substring(startOfLine(source, start), start)
    val chain: List[Node] = ancestors(paragraph)
    val inQuote: Boolean = chain.exists(_.isInstanceOf[BlockQuote])
    val inList: Boolean = chain.exists(node =>
      node.isInstanceOf[ListItem] || node.isInstanceOf[FootnoteBlock] || node.isInstanceOf[DefinitionItem]
    )
    if inQuote && inList && quoteInList.matches(observed) then Some(observed)
    else if inQuote && inList && listInQuote.findPrefixMatchOf(observed).isDefined then
      val quote: String = quoteMarks.findPrefixOf(observed).getOrElse("")
      Some(quote + " " * (ProseLayout.columns(observed) - ProseLayout.columns(quote)))
    else if inQuote && quoteOnly.matches(observed) then Some(observed)
    else if inList && listObserved.matches(observed) then Some(" " * ProseLayout.columns(observed))
    else if !inQuote && !inList && observed.isEmpty then Some("")
    else None

  private val quoteOnly = """^(?:> ?)+$""".r
  private val quoteInList = """^[ ]+> ?(?:> ?)*$""".r
  private val quoteMarks = """^(?:> ?)+""".r
  private val listInQuote = """^(?:> ?)+[ ]*(?:[-*+]|\d{1,9}[.)])[ ]+$""".r
  private val listObserved =
    """^(?:[ ]{0,3}(?:[-*+]|\d{1,9}[.)])(?:[ ]+\[[ xX]\])?[ ]+|\[\^[^\]]+\]:[ ]+|:[ ]+|[ ]*)$""".r

  private def ancestors(node: Node): List[Node] =
    Iterator.unfold(Option(node.getParent)): parent =>
      parent.map(value => (value, Option(value.getParent)))
    .toList

  private def paragraphTokens(paragraph: Node, source: String): List[Token] =
    val kids: List[Node] = children(paragraph)
    val tokens = List.newBuilder[Token]
    var cursor: Int = paragraph.getStartOffset
    var afterSoft: Boolean = false
    kids.foreach: kid =>
      val gap: String = source.substring(cursor, kid.getStartOffset)
      val dropGap: Boolean =
        (gap.nonEmpty && gap.forall(isGapSpace) && kid.isInstanceOf[SoftLineBreak]) ||
          (afterSoft && gap.forall(ch => isGapSpace(ch) || ch == '>'))
      if !dropGap && gap.nonEmpty then tokens ++= splitText(gap)
      afterSoft = false
      kid match
        case _: SoftLineBreak =>
          tokens += Token.Spaces(" ")
          afterSoft = true
        case hard: HardLineBreak =>
          val chars: String = source.substring(hard.getStartOffset, hard.getEndOffset)
          tokens += Token.Hard(chars.takeWhile(_ != '\n'))
        case other =>
          tokens ++= inlineTokens(other, source)
      cursor = kid.getEndOffset
    tokens.result()

  private def isGapSpace(ch: Char): Boolean = ch == ' ' || ch == '\t'

  private def inlineTokens(node: Node, source: String): List[Token] = node match
    case _: Code | _: Link | _: LinkRef | _: Image | _: ImageRef | _: AutoLink | _: MailLink |
         _: HtmlInline | _: HtmlInlineComment | _: HtmlEntity | _: WikiLink | _: WikiImage | _: Footnote =>
      List(Token.Atom(source.substring(node.getStartOffset, node.getEndOffset)))
    case _: Emphasis | _: StrongEmphasis | _: Strikethrough =>
      transparent(node, source)
    case _: Text =>
      splitText(source.substring(node.getStartOffset, node.getEndOffset))
    case _: SoftLineBreak =>
      List(Token.Spaces(" "))
    case hard: HardLineBreak =>
      val chars: String = source.substring(hard.getStartOffset, hard.getEndOffset)
      List(Token.Hard(chars.takeWhile(_ != '\n')))
    case _ =>
      children(node).flatMap(inlineTokens(_, source))

  private def transparent(node: Node, source: String): List[Token] =
    val kids: List[Node] = children(node)
    if kids.isEmpty then List(Token.Atom(source.substring(node.getStartOffset, node.getEndOffset)))
    else
      val inner: List[Token] = kids.flatMap(inlineTokens(_, source))
      val left: String = source.substring(node.getStartOffset, kids.head.getStartOffset)
      val right: String = source.substring(kids.last.getEndOffset, node.getEndOffset)
      ProseLayout.withGlue(inner, left, right)

  private def splitText(text: String): List[Token] =
    def rec(rest: String): List[Token] =
      if rest.isEmpty then Nil
      else
        val (spaces, afterSpaces) = rest.span(isGapSpace)
        if spaces.nonEmpty then Token.Spaces(spaces) :: rec(afterSpaces)
        else
          val (word, afterWord) = afterSpaces.span(ch => !isGapSpace(ch))
          Token.Word(word) :: rec(afterWord)
    rec(text)

  private def children(node: Node): List[Node] =
    Iterator.unfold(Option(node.getFirstChild)): child =>
      child.map(value => (value, Option(value.getNext)))
    .toList

  private def startOfLine(source: String, offset: Int): Int =
    if offset <= 0 then 0
    else
      val newline: Int = source.lastIndexOf('\n', offset - 1)
      if newline < 0 then 0 else newline + 1

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
