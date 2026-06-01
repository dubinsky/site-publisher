package org.podval.tools.publish

import org.podval.tools.publish.PageError
import org.podval.tools.publish.util.{Files, Media, Strings}
import org.podval.xml.{HtmlClass, HtmlXmlDialect, Xml, XmlAttribute, XmlElement}
import zio.blocks.chunk.Chunk
import scala.annotation.tailrec
import scala.jdk.CollectionConverters.SeqHasAsJava
import com.vladsch.flexmark.ext.autolink.AutolinkExtension
import com.vladsch.flexmark.ext.footnotes.FootnoteExtension
//import com.vladsch.flexmark.ext.gfm.strikethrough.StrikethroughSubscriptExtension
import com.vladsch.flexmark.ext.gfm.tasklist.TaskListExtension
import com.vladsch.flexmark.ext.tables.TablesExtension
import com.vladsch.flexmark.html.HtmlRenderer
import com.vladsch.flexmark.parser.Parser
import com.vladsch.flexmark.util.ast.Document
import com.vladsch.flexmark.util.data.MutableDataSet

object MarkdownMarkup extends HtmlLikeMarkup:
  override val extension: String = "md"
  override val additionalExtensions: Set[String] = Set.empty

  private val extensionsCommon: List[Parser.ParserExtension & HtmlRenderer.HtmlRendererExtension] = List(
    FootnoteExtension.create,
//    StrikethroughSubscriptExtension.create,
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
  options.set(FootnoteExtension.FOOTNOTE_LINK_REF_CLASS, Footnotes.LinkClass.name)
  options.set(FootnoteExtension.FOOTNOTE_BACK_LINK_REF_CLASS, Footnotes.BodyClass.name)

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
  ): Xml.Element = HtmlMarkup.parse(
    s"<div>${parseAndRender(content)}</div>",
    errorReporter
  )

  // Note: without FootnotesExtension, FlexMark treats footnotes as links,
  // and by the time we get to `convertFootnotes()` footnotes are gone,
  // so to process footnotes in Markdown markup correctly, I have to enable FootnotesExtension.
  // Here I post-process its output to the form Markup understands.
  override protected def toHtml(element: Xml.Element): Xml.Element =
    // FootnotesExtension footnote link:
    //   <sup id="fnref-$correlationId">
    //     <a class="${Footnotes.LinkClass.name}" href="#fn-$correlationId">
    //       correlationId
    //     </a>
    //   </sup>
    (
      if element.getName != "sup" then None else element
      .getChildren
      .flatMap(_.asElement)
      .find(_.has(Footnotes.LinkClass))
      .map(_.getText)
      .map(Footnotes.linkStub)
    )
    // FootnotesExtension footnote body:
    //   <li id="fn-$correlationId">
    //     ...
    //     <p>...</p>
    //     ...
    //     <a class="Footnotes.LinkBody.name" href="fnref-$correlationId">arrow back symbol</a>
    //     ...
    //   </li>
      .orElse:
        if element.getName != "li" then None else
          val correlationId: Option[String] = element.get(XmlAttribute.Id).flatMap: id =>
            Option.when(id.startsWith("fn-"))(id.substring("fn-".length))

          val body: Option[Xml.Nodes] = Xml
            .getChildren(element)
            .flatMap(_.asElement)
            .find(_.has(Footnotes.BodyClass))
            .map(backLink => element.getChildren.takeWhile(_ ne backLink))

          for
            correlationId <- correlationId
            body <- body
          yield
            // TODO find the <p> within the body and use its children as body...
            Footnotes.bodyStub(correlationId, body)

      .getOrElse(element)

  object WikiLink:
    val startTransclusion: String = "![["
    val startLink: String = "[["
    val end: String = "]]"
    def start(transclude: Boolean): String = if transclude then startTransclusion else startLink
    def text(transclude: Boolean, text: String) = s"${start(transclude)}$text$end"

  object WikiLinkClass extends HtmlClass("wiki-link")
  object WikiBlockClass extends HtmlClass("wiki-block")

  // see https://obsidian.md/help/links
  def convertWikiLinks(text: String): Xml.Nodes = convertWikiLinks(Chunk.empty, text)
  @tailrec
  private def convertWikiLinks(result: Chunk[Xml.Node], text: String): Xml.Nodes =
    if text.isEmpty then result else
      val startTransclusion: Int = text.indexOf(WikiLink.startTransclusion)
      val startLink: Int = text.indexOf(WikiLink.startLink)
      val (start: Int, transclude: Boolean) =
        if startTransclusion == -1 || startTransclusion > startLink
        then (startLink, false)
        else (startTransclusion, true)
      val end: Int = if start == -1 then -1 else text.indexOf(WikiLink.end, start)
      if end == -1 then result ++ Chunk(Xml.text(text)) else
        val before: String = text.substring(0, start)
        val body: String = text.substring(start + WikiLink.start(transclude).length, end).trim
        val after: String = text.substring(end + WikiLink.end.length)
        val (refRaw: String, titleRaw: Option[String]) = Strings.split(body, '|')
        val ref = refRaw.trim
        val title = titleRaw.map(_.trim).filterNot(_.isEmpty)

        var wikiLink: Xml.Element = Xml.element(HtmlXmlDialect.A)
        wikiLink = wikiLink.add(WikiLinkClass)
        if transclude then wikiLink = wikiLink.add(Markup.TranscludeClass)
        if ref.nonEmpty then wikiLink = wikiLink.set(HtmlXmlDialect.Href, ref)
        wikiLink = wikiLink.setText(WikiLink.text(transclude, title.getOrElse(ref)))

        convertWikiLinks(
          result ++ Option.when(before.nonEmpty)(Xml.text(before)).toSeq ++ Chunk(wikiLink),
          after
        )

  private object Footnote:
    val start: String = "[^"
    val end: String = "]"
    val bodyStart: String = ":"

  def convertFootnotes(text: String): Xml.Nodes = convertFootnotes(Chunk.empty, text)
  @tailrec
  // TODO this loop has commonality with the convertWikiLinks() loop...
  private def convertFootnotes(result: Xml.Nodes, text: String): Xml.Nodes =
    if text.isEmpty then result else
      val start: Int = text.indexOf(Footnote.start)
      val end: Int = if start == -1 then -1 else text.indexOf(Footnote.end, start)
      if end == -1 then result ++ Chunk(Xml.text(text)) else
        val before: String = text.substring(0, start)
        val correlationId: String = text.substring(start + Footnote.start.length, end).trim
        val afterRaw: String = text.substring(end + Footnote.end.length)

        val (footnote: Xml.Element, after: String) =
          if !afterRaw.startsWith(Footnote.bodyStart)
          then (Footnotes.linkStub(correlationId), afterRaw)
          // TODO be more precise:
          // - only indented content counts
          // - there may be markup in the footnote body
          else (Footnotes.bodyStub(correlationId, Chunk(Xml.text(afterRaw.substring(Footnote.bodyStart.length).trim))), "")

        convertFootnotes(
          result ++ Option.when(before.nonEmpty)(Xml.text(before)).toSeq ++ Chunk(footnote),
          after
        )

  // TODO according to the Obsidian documentation, block anchor can be added to a "structured block"
  // (e.g., a list) by putting it after the block, with empty lines before and after;
  // I'll deal with this later...
  def setBlockId(element: Xml.Element, errorReporter: PageError.Reporter): Xml.Element =
    val children: Chunk[Xml.Node] = element.getChildren
    if children.isEmpty then element else children.last.asText.fold(element): text =>
      val (before: String, id: Option[String]) = Strings.split(text, '^')
      id.fold(element): id =>
        if before.nonEmpty && !Character.isWhitespace(before.last) then element else
          val result: Xml.Element = element.setChildren(
            children.init ++ Option.when(before.nonEmpty)(Xml.text(before)).toSeq
          )
          result.get(XmlAttribute.Id) match
            case Some(idExisting) =>
              errorReporter.error(PageError.NoId, s"Block id '$id' conflicts with existing id '$idExisting'", result)
            case None =>
              result.set(XmlAttribute.Id, id).add(WikiBlockClass)

  // see https://obsidian.md/help/embeds
  // TODO FlexMark inlines image links for the ![]() references - but does not process image sizes...
  def embed(element: Xml.Element, ref: String): Option[Xml.Element] =
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
