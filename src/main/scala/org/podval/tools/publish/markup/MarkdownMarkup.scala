package org.podval.tools.publish.markup

import org.podval.tools.publish.site.PageErrorReporter
import org.podval.xml.{Html, HtmlXmlDialect, Xml, XmlUtil}
import zio.blocks.chunk.Chunk
import scala.jdk.CollectionConverters.SeqHasAsJava
import com.vladsch.flexmark.ext.autolink.AutolinkExtension
import com.vladsch.flexmark.ext.definition.DefinitionExtension
import com.vladsch.flexmark.ext.footnotes.FootnoteExtension
import com.vladsch.flexmark.ext.gfm.strikethrough.StrikethroughExtension
import com.vladsch.flexmark.ext.gfm.tasklist.TaskListExtension
import com.vladsch.flexmark.ext.tables.TablesExtension
import com.vladsch.flexmark.html.HtmlRenderer
import com.vladsch.flexmark.parser.Parser
import java.io.File

object MarkdownMarkup extends Markup(
  name = "Markdown",
  extension = "md",
  rendersToXml = true,
  xmlDialect = HtmlXmlDialect,
):
  private val extensionsCommon: List[Parser.ParserExtension & HtmlRenderer.HtmlRendererExtension] = List(
    DefinitionExtension.create,
    FootnoteExtension.create,
    StrikethroughExtension.create,
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
    xml: Xml.Element,
    errorReporter: PageErrorReporter
  ): (Xml.Element, Option[Xml.Element]) =
    val result: Xml.Element = xml.transform((element: Xml.Element) =>
      var result: Xml.Element = element
      result = MarkdownWikiBlock.convert(result, errorReporter).getOrElse(result)
      if !result.isA then
        result = XmlUtil.convertText(result, MarkdownWikiLink.convert(Chunk.empty, _))
//      result = convertMarkdownFootnotes(result).getOrElse(result)
      result = convertFootnoteLink(result).getOrElse(result)
      result = convertFootnoteBody(result).getOrElse(result)
      result
    )
    HtmlMarkup.process(
      convert(MarkdownCite.convertElement(result)),
      errorReporter
    )

  private[markup] def convert(xml: Xml.Element): Xml.Element =
    xml.transform((element: Xml.Element) =>
      val children: Xml.Nodes = XmlUtil.convertElements(element.getChildren, HtmlMarkup.unwrapSpuriousParagraph)
      convertAdmonition(convertTaskList(element.setChildren(convertDescriptionLists(children))))
    )

  // FlexMark: <li class="task-list-item"><input class="task-list-item-checkbox" …/>&nbsp;text
  private def convertTaskList(element: Xml.Element): Xml.Element =
    if element.getName != "ul" && element.getName != "ol" then element
    else
      val children: Xml.Nodes = element.getChildren.map: node =>
        node.asElement.filter(_.getName == "li").fold(node)(convertFlexMarkItem)
      TaskList.asList(element.setChildren(children))

  // Obsidian core callouts: `> [!type] Title` (optional `+`/`-` fold). FlexMark emits a blockquote.
  private val admonitionMarker = """\[!([A-Za-z0-9_-]+)\]([+-])?[ \t]*""".r

  private def convertAdmonition(element: Xml.Element): Xml.Element =
    if element.getName != "blockquote" then element
    else obsidianAdmonition(element).getOrElse(element)

  private def obsidianAdmonition(quote: Xml.Element): Option[Xml.Element] =
    val children: List[Xml.Node] = quote.getChildren.filterNot(_.isWhitespace).toList
    for
      first <- children.headOption.flatMap(_.asElement).filter(_.getName == "p")
      (typeName, fold, title, firstBody) <- splitObsidianMarker(first)
    yield Admonition.make(typeName, title, firstBody ++ Chunk.from(children.tail), fold)

  private def splitObsidianMarker(
    paragraph: Xml.Element
  ): Option[(String, Option[Boolean], Option[String], Xml.Nodes)] =
    val nodes: List[Xml.Node] = paragraph.getChildren.toList
    nodes.headOption.flatMap(_.asText).flatMap: text =>
      val nl: Int = text.indexOf('\n')
      val firstLine: String = if nl < 0 then text else text.substring(0, nl)
      val afterNewline: String = if nl < 0 then "" else text.substring(nl + 1)
      admonitionMarker.findPrefixMatchOf(firstLine).map: matched =>
        val typeName: String = matched.group(1).toLowerCase
        val fold: Option[Boolean] = Option(matched.group(2)).map(_ == "+")
        val title: Option[String] = Option(firstLine.substring(matched.end).trim).filter(_.nonEmpty)
        val leftover: List[Xml.Node] =
          val fromText: List[Xml.Node] =
            if afterNewline.isEmpty then Nil else List(Xml.text(afterNewline))
          dropLeadingBreaks(fromText ++ nodes.tail)
        val firstBody: Xml.Nodes =
          if leftover.isEmpty then Chunk.empty
          else Chunk(paragraph.setChildren(Chunk.from(leftover)))
        (typeName, fold, title, firstBody)

  private def dropLeadingBreaks(nodes: List[Xml.Node]): List[Xml.Node] =
    nodes match
      case head :: tail if head.isWhitespace => dropLeadingBreaks(tail)
      case head :: tail if head.asElement.exists(_.getName == "br") => dropLeadingBreaks(tail)
      case other => other

  private def convertFlexMarkItem(li: Xml.Element): Xml.Element =
    val rest: Xml.Nodes = li.getChildren.dropWhile(isIgnorablePrefix)
    rest.headOption.flatMap(_.asElement).filter(TaskList.isCheckbox) match
      case Some(box) => TaskList.asItem(li, box, stripLeadingNbsp(rest.tail))
      case None => li

  private def isIgnorablePrefix(node: Xml.Node): Boolean =
    node.asText.exists(_.forall(c => c.isWhitespace || c == '\u00a0'))

  private def stripLeadingNbsp(nodes: Xml.Nodes): Xml.Nodes =
    nodes.headOption.flatMap(_.asText) match
      case Some(text) if text.nonEmpty && (text.charAt(0) == '\u00a0' || text.charAt(0).isWhitespace) =>
        val trimmed: String = text.dropWhile(c => c == '\u00a0' || c.isWhitespace)
        if trimmed.isEmpty then nodes.tail
        else Xml.text(trimmed) +: nodes.tail
      case _ => nodes

  private val glossaryIal = """\{:\s*\.glossary\s*\}""".r

  private def convertDescriptionLists(nodes: Xml.Nodes): Xml.Nodes =
    var result: List[Xml.Node] = Nil
    var rest: List[Xml.Node] = nodes.toList
    while rest.nonEmpty do
      val node: Xml.Node = rest.head
      rest = rest.tail
      node.asElement.filter(_.getName == "dl") match
        case Some(dl) =>
          val (isGlossary, remaining) = consumeGlossaryMarker(dl, rest)
          rest = remaining
          result = result :+ (if isGlossary then convertDl(dl) else dl)
        case None =>
          result = result :+ node
    Chunk.from(result)

  private def consumeGlossaryMarker(
    dl: Xml.Element,
    rest: List[Xml.Node]
  ): (Boolean, List[Xml.Node]) =
    val markedOnDl: Boolean = dl.hasClass("glossary")
    val (_, remaining) = rest.span(_.isWhitespace)
    remaining.headOption.flatMap(_.asElement).filter(isGlossaryIal) match
      case Some(_) => (true, remaining.tail)
      case None => (markedOnDl, rest)

  private def isGlossaryIal(element: Xml.Element): Boolean =
    element.getName == "p" && glossaryIal.matches(element.getText.trim)

  private def convertDl(dl: Xml.Element): Xml.Element =
    dl.setChildren(DescriptionList.groupItems(dl.getChildren, Glossary.ItemClass, takeTermId))
      .setClasses(dl.getClasses.filterNot(_ == "glossary"))
      .add(Glossary.ListClass)

  private def takeTermId(dt: Xml.Element): (Option[String], Xml.Element) =
    val fromDt: Option[String] = dt.getId.filter(_.nonEmpty)
    val term: Xml.Element = if fromDt.isEmpty then dt else dt.setId("")
    val id: Option[String] = fromDt.orElse:
      val text: String = term.getText.trim
      Option.when(text.nonEmpty)(Xml.toId(text))
    (id, term)

  // Note: without FootnotesExtension, FlexMark treats footnotes as links,
  // and by the time we get to `convertFootnotes()` footnotes are gone -
  // and convertMarkdownFootnotes() does not work.
  // To process footnotes in Markdown markup correctly, I have to enable FootnotesExtension -
  // and convert its output to the form Markup understands (in convertFootnoteLink() and convertFootnoteBody()).

//  private def convertMarkdownFootnotes(element: Xml.Element): Option[Xml.Element] =
//    Option.when(!element.isA)(
//      XmlUtil.convertText(element, convertMarkdownFootnotes(Chunk.empty, _))
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
  //   <a class="footnote-link" footnote-correlation-id="N"/>
  private def convertFootnoteLink(element: Xml.Element): Option[Xml.Element] =
    if element.getName != "sup" then None else
      for correlationId <- element
        .getChildren
        .flatMap(_.asElement)
        .find(_.hasClass("footnote-ref"))
        .map(_.getText)
      yield
        Footnote.link(correlationId)

  // From (FlexMark always wraps the note in <p>, then a sibling backref):
  //   <li id="fn-N">
  //     <p>Footnote Body</p>
  //     <a class="footnote-backref" href="#fnref-N">↩</a>
  //   </li>
  // To:
  //   <span class="footnote" footnote-correlation-id="N">Footnote Body</span>
  private def convertFootnoteBody(element: Xml.Element): Option[Xml.Element] =
    if element.getName != "li" then None else
      for
        correlationId <- element
          .getId
          .flatMap: id =>
            Option.when(id.startsWith("fn-"))(id.substring("fn-".length))
        body <- element
          .getChildren
          .flatMap(_.asElement)
          .find(_.hasClass("footnote-backref"))
          .map(backLink => unwrapFootnoteParagraphs(element.getChildren.takeWhile(_ ne backLink)))
      yield
        Footnote.body(correlationId, body)

  // convert() unwraps lone <p> in td/li/dd, but this <li> is already a footnote span by then.
  private def unwrapFootnoteParagraphs(body: Xml.Nodes): Xml.Nodes =
    val paras: List[Xml.Element] = body
      .filterNot(_.isWhitespace)
      .toList
      .flatMap(_.asElement.filter(_.getName == "p"))
    val significant: Int = body.count(node => !node.isWhitespace)
    if paras.isEmpty || paras.length != significant then body
    else paras.map(_.getChildren).reduce((a, b) => a ++ Chunk(Xml.text(" ")) ++ b)

  override def isSectionHeader(element: Xml.Element): Boolean = HtmlMarkup.isSectionHeader(element)

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
