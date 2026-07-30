package org.podval.tools.publish.markdown

import org.podval.tools.publish.markup.{Converter, Links}
import org.podval.tools.publish.util.Strings
import org.podval.xml.Xml
import zio.blocks.chunk.Chunk
import scala.annotation.tailrec

// see https://obsidian.md/help/links
final class WikiLinksConverter extends Converter:
  override def convert(element: Xml.Element): Option[Xml.Element] =
    Option.when(!element.isA)(
      convertText(element, convertWikiLinks(Chunk.empty, _))
    )

  @tailrec
  private def convertWikiLinks(result: Chunk[Xml.Node], text: String): Xml.Nodes =
    if text.isEmpty then result else
      val startTransclusion: Int = text.indexOf(Links.WikiLink.startTransclusion)
      val startLink: Int = text.indexOf(Links.WikiLink.startLink)
      val (start: Int, transclude: Boolean) =
        if startTransclusion == -1 || startTransclusion > startLink
        then (startLink, false)
        else (startTransclusion, true)
      val end: Int = if start == -1 then -1 else text.indexOf(Links.WikiLink.end, start)
      if end == -1 then result ++ Chunk(Xml.text(text)) else
        val before: String = text.substring(0, start)
        val body: String = text.substring(start + Links.WikiLink.wikiLinkStart(transclude).length, end).trim
        val after: String = text.substring(end + Links.WikiLink.end.length)
        val (refRaw: String, titleRaw: Option[String]) = Strings.split(body, '|')
        val ref = refRaw.trim
        val title = titleRaw.map(_.trim).filterNot(_.isEmpty)
  
        val wikiLink: Xml.Element = Links.wikiLink(
          transclude,
          ref,
          title
        )
        
        convertWikiLinks(
          result ++ Option.when(before.nonEmpty)(Xml.text(before)).toSeq ++ Chunk(wikiLink),
          after
        )
