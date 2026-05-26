package org.podval.tools.publish

import org.podval.tools.publish.PageError
import org.podval.xml.Xml
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
