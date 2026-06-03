package org.podval.tools.publish.features

import org.podval.tools.publish.markup.Markup
import org.podval.tools.publish.util.{Files, Media, Strings}
import org.podval.tools.publish.{Page, PageError}
import org.podval.xml.{HtmlAttribute, HtmlClass, HtmlElement, Xml, XmlAttribute, XmlElement}
import zio.blocks.chunk.Chunk
import scala.annotation.tailrec

// see https://obsidian.md/help/links
object WikiLinksFeature extends Feature:
  private val startTransclusionString: String = "![["
  private val startLinkString: String = "[["
  private val endString: String = "]]"

  private def wikiLinkStart(transclude: Boolean): String = if transclude then startTransclusionString else startLinkString
  def wikiLinkText(transclude: Boolean, text: String) = s"${wikiLinkStart(transclude)}$text$endString"

  object WikiLinkClass extends HtmlClass("wiki-link")

  object TranscludeClass extends HtmlClass("transclude")

  override def process(
    element: Xml.Element,
    context: Feature.ProcessContext
  ): Xml.Element =
    if element.isElement(HtmlElement.A)
    then element
    else convertText(element, convertWikiLinks(Chunk.empty, _))

  @tailrec
  private def convertWikiLinks(result: Chunk[Xml.Node], text: String): Xml.Nodes =
    if text.isEmpty then result else
      val startTransclusion: Int = text.indexOf(startTransclusionString)
      val startLink: Int = text.indexOf(startLinkString)
      val (start: Int, transclude: Boolean) =
        if startTransclusion == -1 || startTransclusion > startLink
        then (startLink, false)
        else (startTransclusion, true)
      val end: Int = if start == -1 then -1 else text.indexOf(endString, start)
      if end == -1 then result ++ Chunk(Xml.text(text)) else
        val before: String = text.substring(0, start)
        val body: String = text.substring(start + wikiLinkStart(transclude).length, end).trim
        val after: String = text.substring(end + endString.length)
        val (refRaw: String, titleRaw: Option[String]) = Strings.split(body, '|')
        val ref = refRaw.trim
        val title = titleRaw.map(_.trim).filterNot(_.isEmpty)

        val wikiLink: Xml.Element = Xml
          .element(HtmlElement.A)
          .add(WikiLinkClass)
          .add(Option.when(transclude)(WikiLinksFeature.TranscludeClass))
          .set(HtmlAttribute.Href, Option.when(ref.nonEmpty)(ref))
          .setText(wikiLinkText(transclude, title.getOrElse(ref)))

        convertWikiLinks(
          result ++ Option.when(before.nonEmpty)(Xml.text(before)).toSeq ++ Chunk(wikiLink),
          after
        )

  override def postProcess(
    element: Xml.Element,
    context: Feature.PostProcessContext
  ): Xml.Element =
    if !element.isElement(HtmlElement.A) || !element.has(WikiLinksFeature.TranscludeClass)
    then element
    else element.get(HtmlAttribute.Href).fold(element)(embed(element, _).getOrElse(element))

  // see https://obsidian.md/help/embeds
  // TODO FlexMark inlines image links for the ![]() references - but does not process image sizes...
  private def embed(element: Xml.Element, ref: String): Option[Xml.Element] =
    Files.nameAndExtension(ref)._2.fold(None): extension =>
      if Media.isImage(extension) then
        val (width: Option[Int], height: Option[Int]) =
          // TODO Embed image, potentially with sizes WIDTHxHEIGHT or just WIDTH or nothing in the text
          (None, None)

        Some(Xml
          .element(XmlElement("img"))
          .set(XmlAttribute("src"), ref)
          .set(XmlAttribute("alt"), s"Image: $ref")
          .set(XmlAttribute("width"), width.map(_.toString))
          .set(XmlAttribute("height"), height.map(_.toString))
        )
      else if Media.isAudio(extension) then Some(Xml
        .element(XmlElement("audio"))
        .set(XmlAttribute("src"), ref)
        .set(XmlAttribute("controls"), true.toString)
      )
      else if extension == "pdf" then
        // TODO Embed PDF viewer, with potentially page=PAGE&height=HEIGHT or one or none in the text
        None
      else
        None
