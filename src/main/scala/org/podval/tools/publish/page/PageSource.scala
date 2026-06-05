package org.podval.tools.publish.page

import org.podval.tools.publish.markup.Markup
import org.podval.tools.publish.page.Page
import org.podval.tools.publish.processor.Features
import org.podval.tools.publish.{PageError, Path, Site}
import org.podval.xml.{Xml, XmlDialect}
import scala.ref.SoftReference

final class PageSource(
  val page: Page,
  val markup: Markup,
  val sourcePath: Path
):
  def site: Site = page.site
  def xmlDialect: XmlDialect = markup.xmlDialect
  def features: Features = markup.features
  
  val errorReporter: PageError.Reporter = PageError.SiteReporter(sourcePath, site)

  private var cachedVar: Option[SoftReference[PageContent]] = None

  def content: PageContent = cachedVar match
    case None => readAndCache("Reading")
    case Some(reference) => reference.get match
      case None => readAndCache("Re-reading evicted")
      case Some(cached) => cached

  // TODO cache XML pre-parsed as part of the disambiguation!
  private def readAndCache(message: String): PageContent =
    site.log.debug(s"$message MarkupSource: $sourcePath")
    val (frontMatterContent: Option[String], markupContent: String) = site.readAndSplit(sourcePath)
    val frontMatter: FrontMatter = FrontMatter.parse(frontMatterContent, errorReporter)
    val xmlParsed: Xml.Element = markup.parse(markupContent, errorReporter)

    val result: PageContent = PageContent(
      this,
      frontMatter,
      xmlParsed,
    )

    cachedVar = Some(SoftReference(result))

    result



