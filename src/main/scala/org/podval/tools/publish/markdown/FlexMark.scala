package org.podval.tools.publish.markdown

import org.podval.tools.publish.markup.Footnotes
import scala.jdk.CollectionConverters.SeqHasAsJava
import com.vladsch.flexmark.ext.autolink.AutolinkExtension
import com.vladsch.flexmark.ext.footnotes.FootnoteExtension
import com.vladsch.flexmark.ext.gfm.tasklist.TaskListExtension
//import com.vladsch.flexmark.ext.gfm.strikethrough.StrikethroughSubscriptExtension
import com.vladsch.flexmark.ext.tables.TablesExtension
import com.vladsch.flexmark.html.HtmlRenderer
import com.vladsch.flexmark.parser.Parser
import com.vladsch.flexmark.util.ast.Document
import com.vladsch.flexmark.util.data.MutableDataSet

object FlexMark:
  def parseAndRenderMarkdown(content: String): String = renderer.render(parseMarkdown(content))

  // Note: FlexMark Parser and Renderer do not throw exceptions on invalid syntax and such.
  private def parseMarkdown(content: String): Document = parser.parse(content)

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
