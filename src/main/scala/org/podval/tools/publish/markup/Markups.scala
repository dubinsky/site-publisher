package org.podval.tools.publish.markup

import org.podval.tools.publish.markdown.{BlocksConverter, FlexMarkFootnotesConverter, KramdownTocHtmlConverter,
  MarkdownFootnotesConverter, MarkdownMarkup, WikiLinksProcessor}
import org.podval.tools.publish.processor.{Processor, SingleProcessor}
import org.podval.tools.publish.tei.{Tei2HtmlConverter, TeiEntityNamesConverter, TeiFacsimileLinksConverter,
  TeiFootnotesConverter, TeiMarkup, TeiSectionIdsConverter}
import zio.blocks.chunk.Chunk

// Known markup languages; note: some XmlLike markups can have extensions other than `.xml`.
final class Markups:
  private var forExtension: Map[String, Markup] = Map.empty

  def forExtension(extension: String): Option[Markup] = forExtension.get(extension)

  private var forElement: Map[String, Markup] = Map.empty

  def forElement(element: String): Option[Markup] = forElement.get(element)

  private var all: Chunk[Markup] = Chunk.empty

  // TODO verify that extensions and root elements do not overlap
  def add(markup: Markup, extensions: Set[String]): Unit =
    forExtension = forExtension ++ extensions.map(_ -> markup)
    forElement = forElement ++ markup.xmlDialect.root.map(_ -> markup)

object Markups:
  private def toProcessors(processors: Seq[Processor]): Seq[SingleProcessor] = processors.flatMap(_.processors)

  def default: Markups =
    val result: Markups = new Markups

    val markdownMarkup: MarkdownMarkup = new MarkdownMarkup(toProcessors(commonProcessors ++ htmlLikeProcessors ++ markdownProcessors))
    result.add(markdownMarkup, Set("md"))

    val htmlMarkup: HtmlMarkup = new HtmlMarkup(toProcessors(commonProcessors ++ htmlLikeProcessors))
    result.add(htmlMarkup, Set(HtmlMarkup.extension))

    val teiMarkup: TeiMarkup = new TeiMarkup(toProcessors(commonProcessors ++ teiProcessors))
    result.add(teiMarkup, Set(XmlLikeMarkup.extension))

    result

  private def commonProcessors: Seq[Processor] = Seq(
    new AnchorIdsConverter,
    new InternalLinksProcessor,
    new FootnotesTransformer
  )

  private def htmlLikeProcessors: Seq[Processor] = Seq(
    new HtmlSectionIdsConverter
  )

  private def markdownProcessors: Seq[Processor] = Seq(
    new BlocksConverter,
    new WikiLinksProcessor,
    new MarkdownFootnotesConverter,
    new FlexMarkFootnotesConverter,
    new KramdownTocHtmlConverter
  )

  private def teiProcessors: Seq[Processor] = Seq(
    new Tei2HtmlConverter,
    new TeiEntityNamesConverter,
    new TeiFacsimileLinksConverter,
    new TeiFootnotesConverter,
    new TeiSectionIdsConverter
  )

