package org.podval.tools.publish

import org.podval.tools.publish.features.{BlocksFeature, Feature}
import org.podval.tools.publish.markup.Markup
import org.podval.tools.publish.util.{Files, IdGenerator}
import org.podval.xml.{Html, Xml, Xml2Html, XmlAst, XmlAttribute}
import scala.ref.SoftReference

object MarkupSource:
  final class Cached(
    val frontMatter: FrontMatter,
    val xml: Xml.Element,
    val toc: Toc
  )

final class MarkupSource(
  val site: Site,
  val markup: Markup,
  val sourcePath: Path
):
  import MarkupSource.Cached

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
      ids = markup.xmlDialect.gather(xml, _.get(XmlAttribute.Id)),
      blocks = markup.xmlDialect.gather(xml, element =>
        if !element.has(BlocksFeature.BlockClass) then None else element.get(XmlAttribute.Id) match
          case None => errorReporter.error(PageError.NoId, s"Defect: No id on block $element", None)
          case Some(id) => Some(Fragment.Block(id))
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
      sortBy = _.processPriority,
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
      sortBy = _.transformPriority,
      action = _.transform(_, _),
      context = Feature.TransformContext(
        xmlDialect = markup.xmlDialect
      )
    )

  def backLinks(page: Page): Seq[BackLinks.BackLink] =
    markup.xmlDialect.gatherWithParents(
      element = cached.xml,
      gatherElement = BackLinks.backLink(_, _, page, cached.toc)
    )

  def htmlContent(page: Page): Html.Element =
    // Post-process XML
    val xmlResult: Xml.Element = process(
      element = cached.xml,
      sortBy = _.postProcessPriority,
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
      sortBy = _.postProcessHtmlPriority,
      action = _.postProcessHtml(_, _),
      context = Feature.PostProcessHtmlContext(
        toc = cached.toc
      )
    )

  private def process[Element: XmlAst, Context](
    element: Element,
    sortBy: Feature => Int,
    context: Context,
    action: (Feature, Element, Context) => Element
  ): Element =
    val featuresSorted: List[Feature] = markup.features.sortBy(sortBy)

    markup.xmlDialect.transform(element, element =>
      featuresSorted.foldLeft(element)((result, feature) =>
        action(feature, result, context)
      )
    )

  private def transform[Element: XmlAst, Context](
    element: Element,
    sortBy: Feature => Int,
    context: Context,
    action: (Feature, Element, Context) => Element
  ): Element =
    val featuresSorted: List[Feature] = markup.features.sortBy(sortBy)

    featuresSorted.foldLeft(element)((result, feature) =>
      action(feature, result, context)
    )
