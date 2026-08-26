package org.podval.tools.publish.markup

import org.podval.tools.publish.util.{Files, Media, Strings}
import org.podval.xml.{HtmlAttribute, HtmlClass, HtmlElement, Xml}
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

  @tailrec
  def convert(result: Chunk[Xml.Node], text: String): Xml.Nodes =
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

  // see https://obsidian.md/help/embeds
  // TODO FlexMark inlines image links for the ![]() references - but does not process image sizes...
  def embed(element: Xml.Element, ref: String): Option[Xml.Element] =
    val (path, _): (String, Option[String]) = Strings.splitFirst(ref, '#')
    val embedded: Option[Xml.Element] = Files.nameAndExtension(path)._2.map(_.toLowerCase) match
      case Some(extension) if Media.isImage(extension) =>
        val (width: Option[Int], height: Option[Int]) =
          // TODO Embed image, potentially with sizes WIDTHxHEIGHT or just WIDTH or nothing in the text
          (None, None)

        Some(Xml
          .element("img")
          .set("src", path)
          .set("alt", s"Image: $path")
          .set("width", width.map(_.toString))
          .set("height", height.map(_.toString))
        )
      case Some(extension) if Media.isAudio(extension) =>
        Some(Xml
          .element("audio")
          .set("src", path)
          .set("controls", true.toString)
        )
      case Some(extension) if Media.isVideo(extension) =>
        Some(Video.make(path, embedLabel(element, path)))
      case Some("pdf") =>
        Some(PdfEmbed.fromRef(ref, embedLabel(element, path)))
      case _ =>
        None
    embedded.map(AssetRef.markWikiEmbed)

  private def embedLabel(element: Xml.Element, path: String): String =
    val inner: String = element.getText.trim.stripPrefix("![[").stripSuffix("]]").trim
    val name: String = Strings.splitFirst(inner, '#')._1.trim
    val fileName: String = path.split('/').lastOption.getOrElse(path)
    val ext: Option[String] = Files.nameAndExtension(name)._2.map(_.toLowerCase)
    if name.isEmpty || name == path || ext.exists(isMediaExtension) then fileName
    else name

  private def isMediaExtension(extension: String): Boolean =
    Media.isImage(extension) || Media.isAudio(extension) || Media.isVideo(extension) || extension == "pdf"
