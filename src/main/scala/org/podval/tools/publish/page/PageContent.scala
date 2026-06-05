package org.podval.tools.publish.page

import org.podval.tools.publish.PageError
import org.podval.tools.publish.feature.Links
import org.podval.tools.publish.link.{BackLink, Fragment, Toc}
import org.podval.tools.publish.util.{Date, IdGenerator}
import org.podval.xml.{Html, Xml, Xml2Html}

final class PageContent private(
  val source: PageSource,
  val frontMatter: FrontMatter,
  val xml: Xml.Element,
  val toc: Toc
):
  def author: Option[String] = frontMatter.author
  def title: Option[String] = frontMatter.title
  def description: Option[String] = frontMatter.description
  def date: Option[Date] = frontMatter.date
  def dateModified: Option[Date] = frontMatter.modifiedTime
  def lang: Option[String] = frontMatter.lang
  
  def backLinks: Seq[BackLink] =
    source.xmlDialect.gatherWithParents(
      element = xml,
      gatherElement = BackLink(_, _, source.page, toc)
    )
    
  def htmlContent: Html.Element =
    // Post-process XML
    val xmlResult: Xml.Element = source.xmlDialect.transform(xml, element =>
      source.features.postConverters.foldLeft(element)((result, postConverter) =>
        postConverter.postConvert(result, this)
      )
    )

    // Convert to HTML
    val htmlResult: Html.Element = Xml2Html.fromXml(xmlResult)

    // Post-process HTML
    source.xmlDialect.transform(htmlResult, element =>
      source.features.htmlConverters.foldLeft(element)((result, htmlConverter) =>
        htmlConverter.convertHtml(result, this)
      )
    )

object PageContent:
  def apply(
    source: PageSource,
    frontMatter: FrontMatter,
    xmlParsed: Xml.Element
  ): PageContent =
    // Run converters
    val ids: IdGenerator = IdGenerator("_generated_id")
    val footnoteCorrelationIds: IdGenerator = IdGenerator("")

    var xml: Xml.Element = source.xmlDialect.transform(xmlParsed, element =>
      source.features.converters.foldLeft(element)((result, converter) =>
        converter.convert(result, source, ids, footnoteCorrelationIds)
      )
    )

    // Run transformers
    xml = source.features.transformers.foldLeft(xml)((result, transformer) =>
      transformer.transform(xml, source)
    )

    val toc: Toc = Toc(
      sections = source.markup.sections(xml, source.errorReporter),
      ids = source.xmlDialect.gather(xml, _.getId),
      blocks = source.xmlDialect.gather(xml, element =>
        if !Links.isBlock(element)
        then None
        else element
          .getId
          .map(Fragment.Block(_))
          .orElse(source.errorReporter.error(
            PageError.NoId,
            s"Defect: No id on block $element",
            None)
          )
      )
    )

    new PageContent(
      source = source,
      frontMatter = frontMatter,
      xml = xml,
      toc = toc
    )
