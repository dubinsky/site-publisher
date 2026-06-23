package org.podval.tools.publish.markdown

import org.podval.tools.publish.markup.Links
import org.podval.tools.publish.processor.{ConverterSimple, PostConverterSimple, Processors}
import org.podval.tools.publish.util.{Files, Media, Strings}
import org.podval.xml.Xml
import zio.blocks.chunk.Chunk
import scala.annotation.tailrec

// see https://obsidian.md/help/links
final class WikiLinksProcessor extends Processors(
  new WikiLinksProcessor.WikiLinksConverter,
  new WikiLinksProcessor.WikiLinksPostConverter
)

object WikiLinksProcessor:
  private final class WikiLinksConverter extends ConverterSimple:
    override def convert(element: Xml.Element): Xml.Element =
      if element.isA
      then element
      else convertText(element, convertWikiLinks(Chunk.empty, _))

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

  private final class WikiLinksPostConverter extends PostConverterSimple:
    override protected def postConvert(element: Xml.Element): Xml.Element =
      if !element.isA || !Links.isTranscluded(element)
      then element
      else element.getHref.fold(element)(embed(element, _).getOrElse(element))

  // see https://obsidian.md/help/embeds
  // TODO FlexMark inlines image links for the ![]() references - but does not process image sizes...
  private def embed(element: Xml.Element, ref: String): Option[Xml.Element] =
    Files.nameAndExtension(ref)._2.fold(None): extension =>
      if Media.isImage(extension) then
        val (width: Option[Int], height: Option[Int]) =
          // TODO Embed image, potentially with sizes WIDTHxHEIGHT or just WIDTH or nothing in the text
          (None, None)

        Some(Xml
          .element("img")
          .set("src", ref)
          .set("alt", s"Image: $ref")
          .set("width", width.map(_.toString))
          .set("height", height.map(_.toString))
        )
      else if Media.isAudio(extension) then Some(Xml
        .element("audio")
        .set("src", ref)
        .set("controls", true.toString)
      )
      else if extension == "pdf" then
        // TODO Embed PDF viewer, with potentially page=PAGE&height=HEIGHT or one or none in the text
        None
      else
        None
