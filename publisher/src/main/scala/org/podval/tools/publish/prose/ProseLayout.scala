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

    def go(rest: List[Token], hold: Hold): Unit =
      rest match
        case Nil => ()
        case Token.Hard(marker) :: tail =>
          appendText(marker)
          if tail.nonEmpty then newline()
          go(tail, Hold.No)
        case Token.Spaces(text) :: tail =>
          val following: String = remainder(tail)
          val sentenceBreak: Boolean = options.sentencePerLine && breakBefore(hold, tail)
          val nextWidth: Int = tail.headOption.fold(0)(tokenColumns)
          val widthBreak: Boolean =
            options.width > 0 && tail.nonEmpty && column + columns(text) + nextWidth > options.width
          val wantBreak: Boolean = (sentenceBreak || widthBreak) && !lineEmpty
          if wantBreak && !leadIn(following) then
            newline()
            go(tail, if sentenceBreak then Hold.No else holdAfterSpace(hold, tail))
          else
            appendText(text)
            go(tail, holdAfterSpace(hold, tail))
        case head :: tail =>
          appendText(visible(head))
          go(tail, holdAfterWord(hold, head, tail, options.sentencePerLine))

    go(tokens, Hold.No)
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

  /**
   * A sentence-ending break waiting to be placed.
   * `Inside` is a parenthetical label, such as `(2)` or `(see above)`, that stays on the sentence.
   * `Closed` is that label just finished: the next space breaks only when a sentence follows.
   */
  private enum Hold derives CanEqual:
    case No
    case Break
    case Inside(depth: Int)
    case Closed

  private val abbreviations: Set[String] = Set(
    "e.g.", "i.e.", "etc.", "vs.", "dr.",
    "mr.", "mrs.", "ms.", "miss.", "prof.", "sr.", "jr.", "st.", "mt.", "rev."
  )
  private val romanNumeral = """(?i)M{0,4}(?:CM|CD|D?C{0,3})(?:XC|XL|L?X{0,3})(?:IX|IV|V?I{0,3})""".r
  private val closers: Set[Int] = Set(
    '"'.toInt, '\u201d'.toInt, '\u2019'.toInt, '\''.toInt, ')'.toInt, '\u00bb'.toInt
  )

  private def breakBefore(hold: Hold, tail: List[Token]): Boolean = hold match
    case Hold.Break => !attachedLabel(tail)
    case Hold.Closed => !attachedLabel(tail) && followingStartsSentence(tail)
    case Hold.Inside(_) | Hold.No => false

  private def holdAfterSpace(hold: Hold, tail: List[Token]): Hold = hold match
    case Hold.Break if attachedLabel(tail) => Hold.Break
    case Hold.Closed if attachedLabel(tail) => Hold.Break
    case Hold.Break | Hold.Closed => Hold.No
    case other => other

  private def holdAfterWord(hold: Hold, token: Token, tail: List[Token], sentencePerLine: Boolean): Hold =
    val ended: Boolean = sentencePerLine && sentenceEnd(token, tail)
    hold match
      case Hold.Inside(depth) => afterLabel(depth, token, ended)
      case Hold.Break if labelOpener(token) => afterLabel(0, token, ended)
      case _ => if ended then Hold.Break else Hold.No

  private def afterLabel(depth: Int, token: Token, ended: Boolean): Hold =
    val depthNow: Int = advanceLabel(depth, visible(token))
    if depthNow > 0 then Hold.Inside(depthNow)
    else if ended then Hold.Break
    else Hold.Closed

  private def advanceLabel(depth: Int, text: String): Int =
    text.foldLeft(depth): (current, ch) =>
      ch match
        case '(' | '[' => current + 1
        case ')' | ']' => math.max(current - 1, 0)
        case _ => current

  private def labelOpener(token: Token): Boolean =
    val text: String = visible(token).dropWhile(isSpaceChar)
    text.startsWith("(") || text.startsWith("[")

  /** A following `(2)`, `[12]`, `(ii)`, or `(see above)` belongs to the sentence that just ended. */
  private def attachedLabel(rest: List[Token]): Boolean =
    val text: String = remainder(rest).dropWhile(isSpaceChar)
    (text.startsWith("(") || text.startsWith("[")) && !startsSentence(labelInterior(text))

  private def labelInterior(text: String): String =
    def rec(rest: String, depth: Int): String =
      if rest.isEmpty || depth == 0 then ""
      else
        val ch: Char = rest.head
        ch match
          case ')' | ']' if depth == 1 => ""
          case ')' | ']' => ch.toString + rec(rest.tail, depth - 1)
          case '(' | '[' => ch.toString + rec(rest.tail, depth + 1)
          case _ => ch.toString + rec(rest.tail, depth)
    rec(text.dropWhile(isSpaceChar).drop(1), 1)

  private def startsSentence(interior: String): Boolean =
    val body: String = interior.dropWhile(_.isWhitespace)
    if body.isEmpty || body.head.isDigit || isRoman(body) then false
    else body.head.isUpper

  private def isRoman(text: String): Boolean =
    text.nonEmpty && text.forall(_.isLetter) && romanNumeral.matches(text)

  private def followingStartsSentence(tail: List[Token]): Boolean =
    nextWord(tail).exists: word =>
      val bare: String = word.dropWhile(isQuoteOrMarkup)
      bare.headOption.exists(ch => ch.isUpper || ch.isDigit) ||
        ((bare.startsWith("(") || bare.startsWith("[")) && startsSentence(labelInterior(bare)))

  private def sentenceEnd(token: Token, tail: List[Token]): Boolean = token match
    case Token.Word(text, left, right) => endsSentence(left + text + right, nextWord(tail))
    case _ => false

  private def nextWord(tail: List[Token]): Option[String] =
    val rest: List[Token] = tail.dropWhile:
      case Token.Spaces(_) => true
      case _ => false
    rest match
      case Token.Word(text, left, right) :: _ => Some(left + text + right)
      case Token.Atom(text) :: _ => Some(text)
      case _ => None

  private def endsSentence(visibleText: String, next: Option[String]): Boolean =
    val core: String = stripClosers(visibleText)
    if core.isEmpty then false
    else if abbreviations.contains(core.toLowerCase) || isDottedAbbreviation(core) || isInitial(core, next) then false
    else core.endsWith("...") || core.endsWith("\u2026") || core.endsWith(".") || core.endsWith("?") || core.endsWith("!")

  /** Pieces of one or two letters: `U.S.`, `Ph.D.`, `a.m.`. A following capital stays on the line. */
  private def isDottedAbbreviation(core: String): Boolean =
    val parts: Array[String] = core.split("\\.", -1)
    parts.length >= 3 && parts.last.isEmpty && parts.dropRight(1).forall: part =>
      part.nonEmpty && part.length <= 2 && part.forall(_.isLetter)

  /**
   * A single letter plus a period is an initial (`Jonas E. Smith`).
   * `I.` before a capital is the pronoun closing a sentence (`you and I. Did`).
   * `Albert I. Jones` still splits.
   */
  private def isInitial(core: String, next: Option[String]): Boolean =
    core.length == 2 && core.last == '.' && core.head.isLetter &&
      !(core == "I." && next.exists(word => word.dropWhile(isQuoteOrMarkup).headOption.exists(_.isUpper)))

  private def isQuoteOrMarkup(ch: Char): Boolean =
    ch == '"' || ch == '\'' || ch == '\u201c' || ch == '\u2018' || ch == '*' || ch == '_' || ch == '~'

  private def isSpaceChar(ch: Char): Boolean = ch == ' ' || ch == '\t'

  /** Optional leading `(`, then a trailing run of closers. */
  private def stripClosers(text: String): String =
    val droppedLead: String = if text.startsWith("(") then text.drop(1) else text
    dropTrailingClosers(droppedLead)

  private def dropTrailingClosers(text: String): String =
    val (_, bodyRev) = text.reverse.span(ch => closers.contains(ch.toInt))
    bodyRev.reverse
