package org.podval.tools.publish.prose

import ProseLayout.Token

/** Mark AsciiDoc inline atoms. There is no inline source tree to walk. */
object AsciiDocScanner:
  def tokens(text: String): List[Token] = parse(text, "")

  private def parse(rest: String, word: String): List[Token] =
    if rest.isEmpty then finish(word, Nil)
    else atom(rest) match
      case Some(consumed, produced) =>
        finish(word, produced ++ parse(rest.substring(consumed), ""))
      case None if rest.nonEmpty && isSpace(rest.charAt(0)) =>
        val (run, tail) = rest.span(isSpace)
        finish(word, Token.Spaces(run) :: parse(tail, ""))
      case None =>
        val size: Int = Character.charCount(rest.codePointAt(0))
        parse(rest.substring(size), word + rest.substring(0, size))

  private def finish(word: String, tail: List[Token]): List[Token] =
    if word.isEmpty then tail else Token.Word(word) :: tail

  private def isSpace(ch: Char): Boolean = ch == ' ' || ch == '\t'

  /** Consumed length and the tokens that replace that slice. */
  private def atom(rest: String): Option[(Int, List[Token])] =
    escaped(rest)
      .orElse(passthrough(rest))
      .orElse(monospace(rest))
      .orElse(footnote(rest))
      .orElse(macroOrAnchor(rest))
      .orElse(urlOrAttribute(rest))
      .orElse(formatting(rest))

  private def escaped(rest: String): Option[(Int, List[Token])] =
    if rest.startsWith("\\") && rest.length > 1 then
      val size: Int = 1 + Character.charCount(rest.codePointAt(1))
      Some((size, List(Token.Atom(rest.substring(0, size)))))
    else None

  private def passthrough(rest: String): Option[(Int, List[Token])] =
    val passMacro = """^pass(?::[a-zA-Z,]+)?\[""".r.findPrefixMatchOf(rest)
    if passMacro.isDefined then bracketAtom(rest, passMacro.get.end)
    else delimited(rest, "+++").orElse(delimited(rest, "$$")).orElse(delimited(rest, "++")).orElse(constrainedPlus(rest))

  private def delimited(rest: String, delim: String): Option[(Int, List[Token])] =
    if rest.startsWith(delim) then
      val close: Int = rest.indexOf(delim, delim.length)
      val end: Int = if close < 0 then rest.length else close + delim.length
      Some((end, List(Token.Atom(rest.substring(0, end)))))
    else None

  private def constrainedPlus(rest: String): Option[(Int, List[Token])] =
    if rest.startsWith("+") && !rest.startsWith("++") && rest.length > 1 && !isSpace(rest.charAt(1)) then
      val close: Int = rest.indexOf('+', 1)
      if close > 1 && !isSpace(rest.charAt(close - 1)) then
        Some((close + 1, List(Token.Atom(rest.substring(0, close + 1)))))
      else None
    else None

  private def monospace(rest: String): Option[(Int, List[Token])] =
    delimited(rest, "``").orElse(delimited(rest, "`"))

  private val footnoteOpen = """^(footnoteref|footnote)(:[^\s\[]*)?\[""".r

  private def footnote(rest: String): Option[(Int, List[Token])] =
    footnoteOpen.findPrefixMatchOf(rest).flatMap: matched =>
      takeBracket(rest.substring(matched.end)).map: (body, consumedAfter) =>
        val marker: String = rest.substring(0, matched.end)
        val inner: String = if body.endsWith("]") then body.dropRight(1) else body
        val bodyTokens: List[Token] = ProseLayout.withGlue(AsciiDocScanner.tokens(inner), marker, "]")
        (matched.end + consumedAfter, bodyTokens)

  private def macroOrAnchor(rest: String): Option[(Int, List[Token])] =
    val macroOpen = """^(\w+)(:{1,2})([^\s\[]*)\[""".r.findPrefixMatchOf(rest)
    if macroOpen.isDefined then bracketAtom(rest, macroOpen.get.end)
    else if rest.startsWith("[[[") then closeAtom(rest, "]]]")
    else if rest.startsWith("[[") then closeAtom(rest, "]]")
    else if rest.startsWith("<<") then closeAtom(rest, ">>")
    else None

  private def bracketAtom(rest: String, openEnd: Int): Option[(Int, List[Token])] =
    takeBracket(rest.substring(openEnd)).map: (_, consumed) =>
      val end: Int = openEnd + consumed
      (end, List(Token.Atom(rest.substring(0, end))))

  private def closeAtom(rest: String, close: String): Option[(Int, List[Token])] =
    val at: Int = rest.indexOf(close, close.length)
    val end: Int = if at < 0 then rest.length else at + close.length
    Some((end, List(Token.Atom(rest.substring(0, end)))))

  private def urlOrAttribute(rest: String): Option[(Int, List[Token])] =
    val pattern = List(
      """^(?:https?|file|ftp|irc)://\S+""".r,
      """^[A-Za-z0-9._%+\-]+@[A-Za-z0-9.\-]+\.[A-Za-z]{2,}""".r,
      """^\{[^\s}]+\}""".r
    ).flatMap(_.findPrefixMatchOf(rest)).headOption
    pattern.map(matched => (matched.end, List(Token.Atom(matched.matched))))

  private val marks: List[String] = List("**", "__", "##", "~~", "^^", "*", "_", "#", "~", "^", "\"`", "'`")

  private def formatting(rest: String): Option[(Int, List[Token])] =
    marks.find(rest.startsWith).flatMap: mark =>
      val close: Int = rest.indexOf(mark, mark.length)
      if close < 0 then None
      else
        val end: Int = close + mark.length
        // A # span stays one atom so a space inside it is not a wrap point.
        val produced: List[Token] =
          if mark == "#" || mark == "##" then List(Token.Atom(rest.substring(0, end)))
          else ProseLayout.withGlue(tokens(rest.substring(mark.length, close)), mark, mark)
        Some((end, produced))

  /** Body including the closing `]`, and how many characters of `rest` that is. `\]` stays literal. */
  private def takeBracket(rest: String): Option[(String, Int)] =
    def rec(pending: String, acc: String): Option[(String, Int)] =
      if pending.isEmpty then None
      else if pending.startsWith("\\]") then rec(pending.substring(2), acc + "\\]")
      else if pending.startsWith("]") then Some((acc + "]", acc.length + 1))
      else
        val size: Int = Character.charCount(pending.codePointAt(0))
        rec(pending.substring(size), acc + pending.substring(0, size))
    rec(rest, "")
