package org.podval.tools.publish

import org.podval.tools.publish.PageError
import org.podval.tools.publish.util.{Files, Media, Strings}
import org.podval.xml.Xml
import zio.blocks.chunk.Chunk
import scala.annotation.tailrec
import scala.jdk.CollectionConverters.SeqHasAsJava
import com.vladsch.flexmark.ext.autolink.AutolinkExtension
import com.vladsch.flexmark.ext.footnotes.FootnoteExtension
import com.vladsch.flexmark.ext.gfm.strikethrough.StrikethroughSubscriptExtension
import com.vladsch.flexmark.ext.gfm.tasklist.TaskListExtension
import com.vladsch.flexmark.ext.tables.TablesExtension
import com.vladsch.flexmark.html.HtmlRenderer
import com.vladsch.flexmark.parser.Parser
import com.vladsch.flexmark.util.ast.Document
import com.vladsch.flexmark.util.data.MutableDataSet

object Markdown extends HtmlLike:
  override val extension: String = "md"
  override val additionalExtensions: Set[String] = Set.empty

  private val extensionsCommon: List[Parser.ParserExtension & HtmlRenderer.HtmlRendererExtension] = List(
    FootnoteExtension.create,
    StrikethroughSubscriptExtension.create,
    TablesExtension.create,
    TaskListExtension.create
  )

  private val extensionsParserOnly: List[Parser.ParserExtension] = List(
    AutolinkExtension.create
  )

  private val extensionsParser: List[Parser.ParserExtension] = extensionsCommon ++ extensionsParserOnly

  private val extensionsRendererOnly: List[HtmlRenderer.HtmlRendererExtension] = List(
  )

  private val extensionsRenderer: List[HtmlRenderer.HtmlRendererExtension] = extensionsCommon ++ extensionsRendererOnly

  private val options: MutableDataSet = new MutableDataSet
//  options.set(Parser.FENCED_CODE_CONTENT_BLOCK, true)

  private val parser: Parser = Parser
    .builder(options)
    .extensions(extensionsParser.asJava)
    .build

  private val renderer: HtmlRenderer = HtmlRenderer
    .builder(options)
    .extensions(extensionsRenderer.asJava)
    .build

  // Note: FlexMark Parser and Renderer do not throw exceptions on invalid syntax and such.
  def parse(content: String): Document = parser.parse(content)
  def parseAndRender(content: String): String = renderer.render(parse(content))

  // Wrap Markdown rendered as HTML in a 'div' and parse.
  override def parse(
    content: String,
    errorReporter: PageError.Reporter
  ): Xml.Element = HtmlLike.Html.parse(
    s"<div>${parseAndRender(content)}</div>",
    errorReporter
  )

  object WikiLink:
    val startTransclusion: String = "![["
    val startLink: String = "[["
    val end: String = "]]"
    def start(transclude: Boolean): String = if transclude then startTransclusion else startLink
    def text(transclude: Boolean, text: String) = s"${start(transclude)}$text$end"

  object WikiLinkClass extends Xml.ClassName("wiki-link")
  object WikiBlockClass extends Xml.ClassName("wiki-block")

  // see https://obsidian.md/help/links
  @tailrec
  def convertWikiLinks(result: Seq[Xml.Xml], text: String): Seq[Xml.Xml] =
    if text.isEmpty then result else
      val startTransclusion: Int = text.indexOf(WikiLink.startTransclusion)
      val startLink: Int = text.indexOf(WikiLink.startLink)
      val (start: Int, transclude: Boolean) =
        if startTransclusion == -1 || startTransclusion > startLink
        then (startLink, false)
        else (startTransclusion, true)
      val end: Int = if start == -1 then -1 else text.indexOf(WikiLink.end, start)
      if end == -1 then result ++ Seq(Xml.mkText(text)) else
        val before: String = text.substring(0, start)
        val body: String = text.substring(start + WikiLink.start(transclude).length, end).trim
        val after: String = text.substring(end + WikiLink.end.length)
        val (refRaw: String, titleRaw: Option[String]) = Strings.split(body, '|')
        val ref = refRaw.trim
        val title = titleRaw.map(_.trim).filterNot(_.isEmpty)

        var wikiLink: Xml.Element = Xml.element(Xml.A.elementName)
        wikiLink = WikiLinkClass.add(wikiLink)
        if transclude then wikiLink = Markup.TranscludeClass.add(wikiLink)
        if ref.nonEmpty then wikiLink = Xml.Href.set(wikiLink, ref)
        wikiLink = Xml.setText(wikiLink, WikiLink.text(transclude, title.getOrElse(ref)))

        convertWikiLinks(
          result ++ Option.when(before.nonEmpty)(Xml.mkText(before)).toSeq ++ Seq(wikiLink),
          after
        )

  // TODO according to the Obsidian documentation, block anchor can be added to a "structured block"
  // (e.g., a list) by putting it after the block, with empty lines before and after;
  // I'll deal with this later...
  def setBlockId(element: Xml.Element, errorReporter: PageError.Reporter): Xml.Element =
    val children: Chunk[Xml.Xml] = Xml.children(element)
    if children.isEmpty then element else Xml.asText(children.last).fold(element): text =>
      val (before: String, id: Option[String]) = Strings.split(text, '^')
      id.fold(element): id =>
        if before.nonEmpty && !Character.isWhitespace(before.last) then element else
          val result: Xml.Element = Xml.setChildren(element,
            children.init ++ Option.when(before.nonEmpty)(Xml.mkText(before)).toSeq
          )
          Xml.Id.get(result) match
            case Some(idExisting) =>
              errorReporter.error(PageError.NoId, s"Block id '$id' conflicts with existing id '$idExisting'", result)
            case None => WikiBlockClass.add(Xml.Id.set(result, id))

  // see https://obsidian.md/help/embeds
  // TODO FlexMark inlines image links for the ![]() references - but does not process image sizes...
  def embed(element: Xml.Element, ref: String): Xml.Element =
    val embedded: Option[Xml.Element] = Files.nameAndExtension(ref)._2.fold(None): extension =>
      if Media.isImage(extension) then
        val (width: Option[Int], height: Option[Int]) =
          // TODO Embed image, potentially with sizes WIDTHxHEIGHT or just WIDTH or nothing in the text
          (None, None)

        var result: Xml.Element = Xml.element("img")
        result = Xml.setAttribute(result, "src", ref)
        result = Xml.setAttribute(result, "alt", s"Image: $ref")
        result = width.fold(result)(width => Xml.setAttribute(result, "width", width.toString))
        result = height.fold(result)(height => Xml.setAttribute(result, "height", height.toString))
        Some(result)
      else if Media.isAudio(extension) then
        var result: Xml.Element = Xml.element("audio")
        result = Xml.setAttribute(result, "src", ref)
        result = Xml.setAttribute(result, "controls", true.toString)
        Some(result)
      else if extension == "pdf" then
        // TODO Embed PDF viewer, with potentially page=PAGE&height=HEIGHT or one or none in the text
        None
      else
        None

    embedded.getOrElse:
      // TODO! can not transclude external links
      element
