package org.podval.tools.publish.page

import org.podval.tools.publish.feature.Links
import org.podval.tools.publish.link.{BackLink, Fragment, Toc}
import org.podval.tools.publish.markup.Markup
import org.podval.tools.publish.page.Page
import org.podval.tools.publish.processor.Features
import org.podval.tools.publish.util.IdGenerator
import org.podval.tools.publish.{PageError, Path, Site}
import org.podval.xml.{Html, Xml, Xml2Html, XmlDialect}
import scala.ref.SoftReference

object PageSource:
  final class Cached(
    val frontMatter: FrontMatter,
    val xml: Xml.Element,
    val toc: Toc
  )

final class PageSource(
  val page: Page,
  val markup: Markup,
  val sourcePath: Path
):
  import PageSource.Cached

  def site: Site = page.site
  def xmlDialect: XmlDialect = markup.xmlDialect
  def features: Features = markup.features
  
  val errorReporter: PageError.Reporter = PageError.SiteReporter(sourcePath, site)

  private var cachedVar: Option[SoftReference[Cached]] = None

  def cached: Cached = cachedVar match
    case None => readAndCache("Reading")
    case Some(reference) => reference.get match
      case None => readAndCache("Re-reading evicted")
      case Some(cached) => cached

  private def readAndCache(message: String): Cached =
    site.log.debug(s"$message MarkupSource: $sourcePath")
    val (frontMatterContent: Option[String], markupContent: String) = site.readAndSplit(sourcePath)
    val frontMatter: FrontMatter = FrontMatter.parse(frontMatterContent, errorReporter)
    val xmlParsed: Xml.Element = markup.parse(markupContent, errorReporter)
    val xml: Xml.Element = process(xmlParsed)
    
    val toc: Toc = Toc(
      sections = markup.sections(xml, errorReporter),
      ids = xmlDialect.gather(xml, _.getId),
      blocks = xmlDialect.gather(xml, element =>
        if !Links.isBlock(element)
        then None
        else element
          .getId
          .map(Fragment.Block(_))
          .orElse(errorReporter.error(PageError.NoId, s"Defect: No id on block $element", None))
      )
    )

    val result: Cached = Cached(
      frontMatter = frontMatter,
      xml = xml,
      toc = toc
    )

    cachedVar = Some(SoftReference(result))

    result

  private def process(element: Xml.Element): Xml.Element =
    // Run converters
    val ids: IdGenerator = IdGenerator("_generated_id")
    val footnoteCorrelationIds: IdGenerator = IdGenerator("")
  
    val result: Xml.Element = xmlDialect.transform(element, element =>
      features.converters.foldLeft(element)((result, converter) =>
        converter.convert(result, this, ids, footnoteCorrelationIds)
      )
    )
  
    // Run transformers
    features.transformers.foldLeft(result)((result, transformer) =>
      transformer.transform(result, this)
    )


  def backLinks: Seq[BackLink] =
    xmlDialect.gatherWithParents(
      element = cached.xml,
      gatherElement = BackLink(_, _, page, cached.toc)
    )

  def htmlContent: Html.Element =
    // Post-process XML
    val xmlResult: Xml.Element = xmlDialect.transform(cached.xml, element =>
      features.postConverters.foldLeft(element)((result, postConverter) =>
        postConverter.postConvert(result, this)
      )
    )

    // Convert to HTML
    val htmlResult: Html.Element = Xml2Html.fromXml(xmlResult)

    // Post-process HTML
    xmlDialect.transform(htmlResult, element =>
      features.htmlConverters.foldLeft(element)((result, htmlConverter) =>
        htmlConverter.convertHtml(result, this)
      )
    )

