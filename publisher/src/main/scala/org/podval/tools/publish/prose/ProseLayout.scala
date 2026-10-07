package org.podval.tools.publish.prose

/** Column wrap and optional sentence breaks. Dialects supply tokens and `leadIn`. */
object ProseLayout:
  enum Token derives CanEqual:
    case Word(text: String, glueLeft: String = "", glueRight: String = "")
    case Atom(text: String)
    case Spaces(text: String)
    case Hard(marker: String)

  def layout(
    tokens: List[Token],
    options: ProseOptions,
    already: Int,
    contPrefix: String,
    leadIn: String => Boolean
  ): String =
    val out = new StringBuilder
    var column: Int = already
    var lineEmpty: Boolean = true
    val contColumns: Int = columns(contPrefix)

    def newline(): Unit =
      out.append('\n').append(contPrefix)
      column = contColumns
      lineEmpty = true

    def appendText(text: String): Unit =
      if text.nonEmpty then
        out.append(text)
        column += columns(text)
        lineEmpty = false

    def go(rest: List[Token], sentenceJustEnded: Boolean): Unit =
      rest match
        case Nil => ()
        case Token.Hard(marker) :: tail =>
          appendText(marker)
          if tail.nonEmpty then newline()
          go(tail, false)
        case Token.Spaces(text) :: tail =>
          val following: String = remainder(tail)
          val sentenceBreak: Boolean = options.sentencePerLine && sentenceJustEnded
          val nextWidth: Int = tail.headOption.fold(0)(tokenColumns)
          val widthBreak: Boolean =
            options.width > 0 && tail.nonEmpty && column + columns(text) + nextWidth > options.width
          val wantBreak: Boolean = (sentenceBreak || widthBreak) && !lineEmpty
          if wantBreak && !leadIn(following) then
            newline()
            go(tail, false)
          else
            appendText(text)
            go(tail, false)
        case head :: tail =>
          appendText(visible(head))
          val ended: Boolean = options.sentencePerLine && sentenceEnd(head)
          go(tail, ended)

    go(tokens, false)
    out.result()

  def visible(token: Token): String = token match
    case Token.Word(text, left, right) => left + text + right
    case Token.Atom(text) => text
    case Token.Spaces(text) => text
    case Token.Hard(marker) => marker

  def columns(text: String): Int = text.codePointCount(0, text.length)

  def withGlue(tokens: List[Token], left: String, right: String): List[Token] =
    def rec(rest: List[Token], seenContent: Boolean): List[Token] = rest match
      case Nil => Nil
      case head :: tail =>
        val content: Boolean = isContent(head)
        val withLeft: Token = if content && !seenContent then glueLeft(head, left) else head
        val later: List[Token] = rec(tail, seenContent || content)
        val glued: Token = if content && !later.exists(isContent) then glueRight(withLeft, right) else withLeft
        glued :: later
    rec(tokens, false)

  private def isContent(token: Token): Boolean = token match
    case Token.Spaces(_) | Token.Hard(_) => false
    case _ => true

  private def glueLeft(token: Token, glue: String): Token = token match
    case word: Token.Word => word.copy(glueLeft = glue + word.glueLeft)
    case Token.Atom(text) => Token.Atom(glue + text)
    case other => other

  private def glueRight(token: Token, glue: String): Token = token match
    case word: Token.Word => word.copy(glueRight = word.glueRight + glue)
    case Token.Atom(text) => Token.Atom(text + glue)
    case other => other

  private def tokenColumns(token: Token): Int = columns(visible(token))

  /** Visible text through the next hard break. The space being broken is not included. */
  private def remainder(rest: List[Token]): String =
    val (segment, _) = rest.span:
      case Token.Hard(_) => false
      case _ => true
    segment.map(visible).mkString

  private val abbreviations: Set[String] = Set("e.g.", "i.e.", "etc.", "vs.", "dr.")
  private val closers: Set[Int] = Set(
    '"'.toInt, '\u201d'.toInt, '\u2019'.toInt, '\''.toInt, ')'.toInt, '\u00bb'.toInt
  )

  private def sentenceEnd(token: Token): Boolean = token match
    case Token.Word(text, left, right) => endsSentence(left + text + right)
    case _ => false

  private def endsSentence(visibleText: String): Boolean =
    val core: String = stripClosers(visibleText)
    if core.isEmpty then false
    else if abbreviations.contains(core.toLowerCase) then false
    else core.endsWith("...") || core.endsWith("\u2026") || core.endsWith(".") || core.endsWith("?") || core.endsWith("!")

  /** Optional leading `(`, then a trailing run of closers. */
  private def stripClosers(text: String): String =
    val droppedLead: String = if text.startsWith("(") then text.drop(1) else text
    dropTrailingClosers(droppedLead)

  private def dropTrailingClosers(text: String): String =
    val (_, bodyRev) = text.reverse.span(ch => closers.contains(ch.toInt))
    bodyRev.reverse
