package org.podval.tools.publish.markup

import org.podval.tools.publish.util.Strings
import org.podval.xml.Xml
import scala.annotation.tailrec

object MarkdownWikiLink:
  @tailrec
  def convert(result: Xml.Nodes, text: String): Xml.Nodes =
    if text.isEmpty then result else
      val startTransclusion: Int = text.indexOf(WikiLink.startTransclusion)
      val startLink: Int = text.indexOf(WikiLink.startLink)
      val (start: Int, transclude: Boolean) =
        if startTransclusion == -1 || startTransclusion > startLink
        then (startLink, false)
        else (startTransclusion, true)
      val end: Int = if start == -1 then -1 else text.indexOf(WikiLink.end, start)
      if end == -1 then result ++ Seq(Xml.text(text)) else
        val before: String = text.substring(0, start)
        val body: String = text.substring(start + WikiLink.wikiLinkStart(transclude).length, end).trim
        val after: String = text.substring(end + WikiLink.end.length)
        val (refRaw: String, titleRaw: Option[String]) = Strings.split(body, '|')
        val ref = refRaw.trim
        val title = titleRaw.map(_.trim).filterNot(_.isEmpty)
        convert(
          result ++ Option.when(before.nonEmpty)(Xml.text(before)).toSeq ++ Seq(WikiLink.make(transclude, ref, title)),
          after
        )
