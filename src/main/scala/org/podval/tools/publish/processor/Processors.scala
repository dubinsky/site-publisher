package org.podval.tools.publish.processor

import org.podval.tools.publish.page.PageContent
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

  def process(pageContent: PageContent): Xml.Element =
    // Run converters
    val ids: IdGenerator = IdGenerator("_generated_id")
    val footnoteCorrelationIds: IdGenerator = IdGenerator("")

    val converted: Xml.Element = pageContent.xmlDialect.transform(pageContent.xml, element =>
      converters.foldLeft(element)((result, converter) => converter
        .convert(result, pageContent, ids, footnoteCorrelationIds)
        .getOrElse(result)
      )
    )

    // Run transformers
    transformers.foldLeft(converted)((result, transformer) =>
      transformer.transform(result, pageContent)
    )

  private lazy val postConverters: Seq[PostConverter] = processors
    .collect { case postConverter: PostConverter => postConverter }

  private lazy val htmlConverters: Seq[HtmlConverter] = processors
    .collect { case htmlConverter: HtmlConverter => htmlConverter }

  def toHtml(pageContent: PageContent): Html.Element =
    // Post-process XML
    val xmlResult: Xml.Element = pageContent.xmlDialect.transform(pageContent.xml, element =>
      postConverters.foldLeft(element)((result, postConverter) => postConverter
        .postConvert(result, pageContent)
        .getOrElse(result)
      )
    )

    // Convert to HTML
    val htmlResult: Html.Element = Xml2Html.fromXml(xmlResult)

    // Post-process HTML
    pageContent.xmlDialect.transform(htmlResult, element =>
      htmlConverters.foldLeft(element)((result, htmlConverter) =>
        htmlConverter.convertHtml(result, pageContent)
      )
    )
