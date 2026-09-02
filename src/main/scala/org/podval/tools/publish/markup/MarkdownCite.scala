package org.podval.tools.publish.markup

import org.podval.xml.{Xml, XmlUtil}
import scala.annotation.tailrec

object MarkdownCite:
  def convertElement(element: Xml.Element): Xml.Element =
    val converted: Xml.Element =
      if isBibliographyFence(element)
      then Citation.listPlaceholder
      else XmlUtil.convertText(element, convert(Seq.empty, _))
    converted.setChildren(converted.getChildren.map(child =>
      child.asElement.fold(child)(convertElement)
    ))

  private def isBibliographyFence(element: Xml.Element): Boolean =
    element.getName == "p" && element.getText.trim == ":::bibliography"

  @tailrec
  def convert(result: Xml.Nodes, text: String): Xml.Nodes =
    if text.isEmpty then result
    else
      val bracket: Int = text.indexOf("[@")
      val bracketSuppress: Int = text.indexOf("[-@")
      val nextBracket: Int =
        if bracket == -1 then bracketSuppress
        else if bracketSuppress == -1 then bracket
        else Math.min(bracket, bracketSuppress)
      val narrative: Int = findNarrative(text)
      val (start: Int, kind: Kind) =
        if nextBracket == -1 && narrative == -1 then (-1, Kind.Parenthetical)
        else if nextBracket == -1 then (narrative, Kind.Narrative)
        else if narrative == -1 || nextBracket <= narrative then
          (nextBracket, if text.startsWith("[-@", nextBracket) then Kind.Suppress else Kind.Parenthetical)
        else (narrative, Kind.Narrative)
      if start == -1 then result ++ Seq(Xml.text(text))
      else
        val before: String = text.substring(0, start)
        val (cite, after) = kind match
          case Kind.Narrative =>
            val keyEnd: Int = keyEndIndex(text, start + 1)
            val key: String = text.substring(start + 1, keyEnd)
            (Citation.cite(Citation.Mode.Narrative, Seq(Citation.Item(key))), text.substring(keyEnd))
          case Kind.Parenthetical | Kind.Suppress =>
            val close: Int = text.indexOf(']', start)
            if close == -1 then (Xml.text(text.substring(start)), "")
            else
              val inner: String = text.substring(start + 1, close)
              val mode: Citation.Mode =
                if kind == Kind.Suppress || inner.startsWith("-@") then Citation.Mode.SuppressAuthor
                else Citation.Mode.Parenthetical
              val items: Seq[Citation.Item] = parseBracket(inner)
              (Citation.cite(mode, items), text.substring(close + 1))
        convert(
          result ++ Option.when(before.nonEmpty)(Xml.text(before)).toSeq ++ Seq(cite),
          after
        )

  private enum Kind derives CanEqual:
    case Parenthetical, Narrative, Suppress

  private def findNarrative(text: String): Int =
    var i: Int = 0
    while i < text.length do
      if text.charAt(i) == '@' && (i == 0 || text.charAt(i - 1).isWhitespace || "([{\"'".contains(text.charAt(i - 1))) then
        val keyStart: Int = i + 1
        if keyStart < text.length && text.charAt(keyStart).isLetter then
          val keyEnd: Int = keyEndIndex(text, keyStart)
          val key: String = text.substring(keyStart, keyEnd)
          if Citation.isBibKey(key) && !looksLikeEmail(text, i, keyEnd) then return i
      i += 1
    -1

  private def keyEndIndex(text: String, from: Int): Int =
    var i: Int = from
    while i < text.length && (text.charAt(i).isLetterOrDigit || "_-:".contains(text.charAt(i))) do i += 1
    i

  private def looksLikeEmail(text: String, at: Int, keyEnd: Int): Boolean =
    at > 0 && text.charAt(at - 1).isLetterOrDigit && keyEnd < text.length && text.charAt(keyEnd) == '.'

  private def parseBracket(inner: String): Seq[Citation.Item] =
    val body: String = inner.stripPrefix("-").trim
    body.split(';').toSeq.map(_.trim).filter(_.nonEmpty).flatMap: piece =>
      val token: String = piece.stripPrefix("@").trim
      val comma: Int = token.indexOf(',')
      if comma == -1 then
        Option.when(Citation.isBibKey(token))(Citation.Item(token))
      else
        val key: String = token.substring(0, comma).trim
        val locator: String = token.substring(comma + 1).trim
        Option.when(Citation.isBibKey(key))(Citation.Item(key, Option.when(locator.nonEmpty)(locator)))
