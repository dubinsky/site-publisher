package org.podval.tools.publish.markup

import org.podval.tools.publish.link.Toc
import org.podval.tools.publish.markdown.MarkdownMarkup
import org.podval.tools.publish.page.PageSource
import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.{Html, Xml, Xml2Html}

final class Markup(
  val kind: MarkupKind,
  processors: Seq[Processor]
):
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

    val converted: Xml.Element = kind.xmlDialect.transform(xml, element =>
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
    toc: Toc,
    doAddToc: Boolean
  ): Html.Element =
    // Post-process XML
    val xmlResult: Xml.Element = kind.xmlDialect.transform(xml, element =>
      postConverters.foldLeft(element)((result, postConverter) => postConverter
        .postConvert(result, source)
        .getOrElse(result)
      )
    )

    // Convert to HTML
    val htmlResult: Html.Element = Xml2Html.fromXml(xmlResult)

    // Add TOC to HTML
    var tocAdded: Boolean = false
    source.xmlDialect.transform(htmlResult, element =>
      if tocAdded || !kind.isTocPlaceholder(element)
      then
        element
      else
        tocAdded = true
        toc.html
    )

    if doAddToc && !tocAdded
    then htmlResult.setChildren(toc.html +: htmlResult.getChildren)
    else htmlResult

