package org.podval.tools.publish.page

import org.podval.tools.publish.feature.{Feature, Links}
import org.podval.tools.publish.link.{BackLink, Fragment, Toc}
import org.podval.tools.publish.markup.Markup
import org.podval.tools.publish.page.Page
import org.podval.tools.publish.util.IdGenerator
import org.podval.tools.publish.{PageError, Path, Site}
import org.podval.xml.{Html, Xml, Xml2Html, XmlAst}
import scala.ref.SoftReference

object PageSource:
  final class Cached(
    val frontMatter: FrontMatter,
    val xml: Xml.Element,
    val toc: Toc
  )

final class PageSource(
  val site: Site,
  val markup: Markup,
  val sourcePath: Path
):
  import PageSource.Cached

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
    val xml: Xml.Element = process(xmlParsed, markup)

    val toc: Toc = Toc(
      sections = markup.sections(xml, errorReporter),
      ids = markup.xmlDialect.gather(xml, _.getId),
      blocks = markup.xmlDialect.gather(xml, element =>
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

  private def process(
    xmlParsed: Xml.Element,
    markup: Markup
  ): Xml.Element =
    // Process XML
    val xml: Xml.Element = process(
      element = xmlParsed,
      runLast = Some(_.processesLinks),
      action = _.process(_, _),
      context = Feature.ProcessContext(
        ids = IdGenerator("_generated_id"),
        siteUrl = site.config.url,
        errorReporter = errorReporter
      )
    )

    // Transform XML
    transform(
      element = xml,
      runLast = Some(_.transformsFootnotes),
      action = _.transform(_, _),
      context = Feature.TransformContext(
        xmlDialect = markup.xmlDialect
      )
    )

  def backLinks(page: Page): Seq[BackLink] =
    markup.xmlDialect.gatherWithParents(
      element = cached.xml,
      gatherElement = BackLink(_, _, page, cached.toc)
    )

  def htmlContent(page: Page): Html.Element =
    // Post-process XML
    val xmlResult: Xml.Element = process(
      element = cached.xml,
      runLast = None,
      action = _.postProcess(_, _),
      context = Feature.PostProcessContext(
        page = page,
        errorReporter = errorReporter
      )
    )

    // Convert to HTML
    val htmlResult: Html.Element = Xml2Html.fromXml(xmlResult)

    // Post-process HTML
    process(
      element = htmlResult,
      runLast = None,
      action = _.postProcessHtml(_, _),
      context = Feature.PostProcessHtmlContext(
        toc = cached.toc
      )
    )

  private def process[Element: XmlAst, Context](
    element: Element,
    runLast: Option[Feature => Boolean],
    context: Context,
    action: (Feature, Element, Context) => Element
  ): Element =
    val featuresSorted: List[Feature] = sortFeatures(runLast)

    markup.xmlDialect.transform(element, element =>
      featuresSorted.foldLeft(element)((result, feature) =>
        action(feature, result, context)
      )
    )

  private def transform[Element: XmlAst, Context](
    element: Element,
    runLast: Option[Feature => Boolean],
    context: Context,
    action: (Feature, Element, Context) => Element
  ): Element =
    val featuresSorted: List[Feature] = sortFeatures(runLast)

    featuresSorted.foldLeft(element)((result, feature) =>
      action(feature, result, context)
    )

  private def sortFeatures(runLast: Option[Feature => Boolean]): List[Feature] =
    val features: List[Feature] = markup.features
    runLast.fold(features)(features.sortBy)
