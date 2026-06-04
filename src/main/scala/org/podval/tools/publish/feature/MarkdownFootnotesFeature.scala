package org.podval.tools.publish.feature

import org.podval.tools.publish.PageError
import org.podval.xml.{HtmlElement, Xml}
import zio.blocks.chunk.Chunk
import scala.annotation.tailrec

object MarkdownFootnotesFeature extends Feature:
  private val startsString: String = "[^"
  private val endString: String = "]"
  private val bodyStartString: String = ":"

  override def process(
    element: Xml.Element,
    context: Feature.ProcessContext
  ): Xml.Element =
    if element.isA
    then element
    else convertText(element, convertFootnotes(Chunk.empty, _))

  @tailrec
  // TODO this loop has commonality with the WikiLinksFeature.convertWikiLinks() loop...
  private def convertFootnotes(result: Xml.Nodes, text: String): Xml.Nodes =
    if text.isEmpty then result else
      val start: Int = text.indexOf(startsString)
      val end: Int = if start == -1 then -1 else text.indexOf(endString, start)
      if end == -1 then result ++ Chunk(Xml.text(text)) else
        val before: String = text.substring(0, start)
        val correlationId: String = text.substring(start + startsString.length, end).trim
        val afterRaw: String = text.substring(end + endString.length)

        val (footnote: Xml.Element, after: String) =
          if !afterRaw.startsWith(bodyStartString)
          then (Footnotes.linkStub(correlationId), afterRaw)
          // TODO be more precise:
          // - only indented content counts
          // - there may be markup in the footnote body
          else (Footnotes.bodyStub(correlationId, Chunk(Xml.text(afterRaw.substring(bodyStartString.length).trim))), "")

        convertFootnotes(
          result ++ Option.when(before.nonEmpty)(Xml.text(before)).toSeq ++ Chunk(footnote),
          after
        )
