package org.podval.tools.publish.markup

import org.podval.tools.publish.page.PageSource
import org.podval.xml.{Html, HtmlXmlDialect, Xml}
//import zio.blocks.chunk.Chunk
//import scala.annotation.tailrec
import scala.jdk.CollectionConverters.SeqHasAsJava
import com.vladsch.flexmark.ext.autolink.AutolinkExtension
import com.vladsch.flexmark.ext.footnotes.FootnoteExtension
import com.vladsch.flexmark.ext.gfm.tasklist.TaskListExtension
import com.vladsch.flexmark.ext.tables.TablesExtension
import com.vladsch.flexmark.html.HtmlRenderer
import com.vladsch.flexmark.parser.Parser
import java.io.File

object MarkdownMarkup extends Markup(
  name = "Markdown",
  allowsInternalFrontMatter = true,
  extension = "md",
  rendersToXml = true,
  xmlDialect = HtmlXmlDialect,
):
  private val extensionsCommon: List[Parser.ParserExtension & HtmlRenderer.HtmlRendererExtension] = List(
    FootnoteExtension.create,
    TablesExtension.create,
    TaskListExtension.create
  )

  private val extensionsParser: List[Parser.ParserExtension] = extensionsCommon ++ List(
    AutolinkExtension.create
  )
  private val extensionsRenderer: List[HtmlRenderer.HtmlRendererExtension] = extensionsCommon ++ List(
  )

  private val parser: Parser = Parser
    .builder()
    .extensions(extensionsParser.asJava)
    .build

  private val renderer: HtmlRenderer = HtmlRenderer
    .builder()
    .extensions(extensionsRenderer.asJava)
    .build

  // Note: FlexMark Parser and Renderer do not throw exceptions on invalid syntax and such.
  def parseAndRenderMarkdown(content: String): String = renderer.render(parser.parse(content))

  override def xmlContent(content: String, sourceFile: File): String =
    // Wrap Markdown rendered as HTML in a 'div'.
    s"<div>${parseAndRenderMarkdown(content)}</div>"

  override def process(
    source: PageSource,
    xml: Xml.Element
  ): (Xml.Element, Option[Xml.Element]) =
    val result: Xml.Element = xmlDialect.transform(xml, (element: Xml.Element) =>
      var result: Xml.Element = element
      result = Blocks.convertBlock(result, source).getOrElse(result)
      result = Links.convertWikiLinks(result).getOrElse(result)
//      result = convertMarkdownFootnotes(result).getOrElse(result)
      result = convertFootnoteLink(result).getOrElse(result)
      result = convertFootnoteBody(result).getOrElse(result)
      result
    )
    HtmlMarkup.process(
      source,
      result
    )

  // Note: without FootnotesExtension, FlexMark treats footnotes as links,
  // and by the time we get to `convertFootnotes()` footnotes are gone -
  // and convertMarkdownFootnotes() does not work.
  // To process footnotes in Markdown markup correctly, I have to enable FootnotesExtension -
  // and convert its output to the form Markup understands (in convertFootnoteLink() and convertFootnoteBody()).

//  private def convertMarkdownFootnotes(element: Xml.Element): Option[Xml.Element] =
//    Option.when(!element.isA)(
//      // TODO move to XmlUtils
//      element.setChildren(element.getChildren.flatMap(xml => xml.asText.fold(Seq(xml))(convertMarkdownFootnotes(Chunk.empty, _))))
//    )
//
//  private val startsString: String = "[^"
//  private val endString: String = "]"
//  private val bodyStartString: String = ":"
//
//  @tailrec
//  // TODO this loop has commonality with the WikiLinksFeature.convertWikiLinks() loop...
//  private def convertMarkdownFootnotes(result: Xml.Nodes, text: String): Xml.Nodes =
//    if text.isEmpty then result else
//      val start: Int = text.indexOf(startsString)
//      val end: Int = if start == -1 then -1 else text.indexOf(endString, start)
//      if end == -1 then result ++ Chunk(Xml.text(text)) else
//        val before: String = text.substring(0, start)
//        val correlationId: String = text.substring(start + startsString.length, end).trim
//        val afterRaw: String = text.substring(end + endString.length)
//
//        val (footnote: Xml.Element, after: String) =
//          if !afterRaw.startsWith(bodyStartString)
//          then (Footnotes.linkStub(correlationId), afterRaw)
//          // TODO be more precise:
//          // - only indented content counts
//          // - there may be markup in the footnote body
//          else (Footnotes.bodyStub(correlationId, Chunk(Xml.text(afterRaw.substring(bodyStartString.length).trim))), "")
//
//        convertMarkdownFootnotes(
//          result ++ Option.when(before.nonEmpty)(Xml.text(before)).toSeq ++ Chunk(footnote),
//          after
//        )

  // From:
  //   <sup id="fnref-N"><a class="footnote-ref" href="#fn-N">N</a></sup>
  // To:
  //   <a class="footnote-link" footnoteCorrelationId="N"/>
  private def convertFootnoteLink(element: Xml.Element): Option[Xml.Element] =
    if element.getName != "sup" then None else
      for correlationId <- element
        .getChildren
        .flatMap(_.asElement)
        .find(_.hasClass("footnote-ref"))
        .map(_.getText)
      yield
        Footnotes.linkStub(correlationId)

  // From:
  //   <li id="fn-N">
  //     ...
  //     <p>...</p>
  //     ...
  //     <a class="footnote-backref" href="fnref-N">Footnote Body</a>
  //     ...
  //   </li>
  // To:
  //   <span class="footnote" footnoteCorrelationId="N">Footnote Body</span>
  private def convertFootnoteBody(element: Xml.Element): Option[Xml.Element] =
    if element.getName != "li" then None else
      for
        correlationId <- element
          .getId
          .flatMap: id =>
            Option.when(id.startsWith("fn-"))(id.substring("fn-".length))
        body <- Xml
          .getChildren(element)
          .flatMap(_.asElement)
          .find(_.hasClass("footnote-backref"))
          .map(backLink => element.getChildren.takeWhile(_ ne backLink))
      yield
        // TODO find the <p> within the body and use its children as body...
        Footnotes.bodyStub(correlationId, body)

  override def postProcess(source: PageSource, xml: Xml.Element): Xml.Element =
    Links.embedWikiLinks(xml, xmlDialect)
  
  override def isSpuriousFootnotesDiv(element: Xml.Element): Boolean =
    element.getName == "div" && element.hasClass("footnotes")

  // Kramdown Toc Marker
  override def isTocPlaceholder(element: Html.Element): Boolean =
    element.getName == "ul" && element.getChildren.exists: node =>
      node.asElement.fold(false): child =>
        child.getName == "li" &&
        child.getChildren.length == 1 &&
        child.getChildren.head.asText.fold(false): text =>
          text.endsWith("{:toc}")
