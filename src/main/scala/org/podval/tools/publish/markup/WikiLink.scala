package org.podval.tools.publish.markup

import org.podval.tools.publish.util.{Files, Media, Strings}
import org.podval.xml.{HtmlAttribute, HtmlClass, HtmlElement, Xml, XmlDialect}
import zio.blocks.chunk.Chunk
import scala.annotation.tailrec

// for wiki links syntax, see https://obsidian.md/help/links
object WikiLink:
  private object WikiLinkClass extends HtmlClass("wiki-link")

  private object TranscludeClass extends HtmlClass("transclude")

  def isTranscluded(element: Xml.Element): Boolean = element.has(TranscludeClass)

  private def wikiLink(
    transclude: Boolean,
    ref: String,
    title: Option[String]
  ): Xml.Element = Xml
    .element(HtmlElement.A)
    .add(WikiLinkClass)
    .add(Option.when(transclude)(TranscludeClass))
    .set(HtmlAttribute.Href, Option.when(ref.nonEmpty)(ref))
    .setText(wikiLinkText(transclude, title.getOrElse(ref)))

  private val startTransclusionStr: String = "![["
  private val startLinkStr: String = "[["
  private val endStr: String = "]]"
  private def wikiLinkStart(transclude: Boolean): String = if transclude then startTransclusionStr else startLinkStr
  private def wikiLinkText(transclude: Boolean, text: String) = s"${wikiLinkStart(transclude)}$text$endStr"

  def linkText(element: Xml.Element, text: String): String =
    if element.has(WikiLinkClass)
    then wikiLinkText(isTranscluded(element), text)
    else text

  // TODO unfold
  def convert(element: Xml.Element): Option[Xml.Element] =
    Option.when(!element.isA)(
      // TODO move to XmlUtils
      element.setChildren(element.getChildren.flatMap(xml => xml.asText.fold(Seq(xml))(convert(Chunk.empty, _))))
    )

  @tailrec
  private def convert(result: Chunk[Xml.Node], text: String): Xml.Nodes =
    if text.isEmpty then result else
      val startTransclusion: Int = text.indexOf(startTransclusionStr)
      val startLink: Int = text.indexOf(startLinkStr)
      val (start: Int, transclude: Boolean) =
        if startTransclusion == -1 || startTransclusion > startLink
        then (startLink, false)
        else (startTransclusion, true)
      val end: Int = if start == -1 then -1 else text.indexOf(endStr, start)
      if end == -1 then result ++ Chunk(Xml.text(text)) else
        val before: String = text.substring(0, start)
        val body: String = text.substring(start + wikiLinkStart(transclude).length, end).trim
        val after: String = text.substring(end + endStr.length)
        val (refRaw: String, titleRaw: Option[String]) = Strings.split(body, '|')
        val ref = refRaw.trim
        val title = titleRaw.map(_.trim).filterNot(_.isEmpty)
  
        val wikiLink: Xml.Element = WikiLink.wikiLink(
          transclude,
          ref,
          title
        )
        
        convert(
          result ++ Option.when(before.nonEmpty)(Xml.text(before)).toSeq ++ Chunk(wikiLink),
          after
        )

  // TODO unfold
  def embed(xml: Xml.Element, xmlDialect: XmlDialect): Xml.Element =
    xmlDialect.transform(xml, element => Option
      .when(element.isA && isTranscluded(element))(
        element.getHref.fold(element)(embed(element, _).getOrElse(element))
      )
      .getOrElse(element)
    )

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
