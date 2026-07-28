package org.podval.tools.publish.processor

import org.podval.tools.publish.link.Toc
import org.podval.tools.publish.markdown.MarkdownMarkup
import org.podval.tools.publish.page.PageSource
import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.{Html, Xml, Xml2Html}

// TODO rename to Pipeline?
final class Processors(
  processors: Seq[Processor]
) derives CanEqual:
  private lazy val converters: Seq[Converter] = processors
    .collect { case converter: Converter => converter }
    .sortBy(_.stage.ordinal)

  private lazy val transformers: Seq[Transformer] = processors
    .collect { case transformer: Transformer => transformer }
    .sortBy(_.stage.ordinal)

  def process(source: PageSource, xml: Xml.Element): Xml.Element =
    // Run converters
    val ids: IdGenerator = IdGenerator("_generated_id")
    val footnoteCorrelationIds: IdGenerator = IdGenerator("")

    val converted: Xml.Element = source.xmlDialect.transform(xml, element =>
      converters.foldLeft(element)((result, converter) => converter
        .convert(result, source, ids, footnoteCorrelationIds)
        .getOrElse(result)
      )
    )

    // Run transformers
    transformers.foldLeft(converted)((result, transformer) =>
      transformer.transform(result, source)
    )

  private lazy val postConverters: Seq[PostConverter] = processors
    .collect { case postConverter: PostConverter => postConverter }
  
  def toHtml(
    source: PageSource,
    xml: Xml.Element,
    toc: Toc
  ): Html.Element =
    // Post-process XML
    val xmlResult: Xml.Element = source.xmlDialect.transform(xml, element =>
      postConverters.foldLeft(element)((result, postConverter) => postConverter
        .postConvert(result, source)
        .getOrElse(result)
      )
    )

    // Convert to HTML
    val htmlResult: Html.Element = Xml2Html.fromXml(xmlResult)

    // Add TOC to HTML
    source.xmlDialect.transform(htmlResult, element =>
      // TODO only when relevant
      // TODO if must add and did not add, add at the head
      if !MarkdownMarkup.isKramdownTocMarker(element)
      then element
      else toc.html
  )
