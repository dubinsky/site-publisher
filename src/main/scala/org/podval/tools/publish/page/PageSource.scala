package org.podval.tools.publish.page

import org.podval.tools.publish.markup.{Markup, MarkupKind}
import org.podval.tools.publish.{PageError, Path, Site}
import org.podval.xml.{Xml, XmlDialect}
import scala.ref.SoftReference

final class PageSource(
  val page: OriginalMarkupPage,
  val markup: Markup,
  val sourcePath: Path,
  standAloneFrontMatter: Option[Path]
):
  def site: Site = page.site
  def markupKind: MarkupKind = markup.kind
  def xmlDialect: XmlDialect = markupKind.xmlDialect

  private var cachedVar: Option[SoftReference[PageContent]] = None
  
  def cache(frontMatter: FrontMatter, xml: Xml.Element): PageContent =
    val result: PageContent = PageContent(
      source = this,
      frontMatter = frontMatter,
      xml = markup.processors.process(this, xml),
    )

    cachedVar = Some(SoftReference(result))

    result
  
  def content: PageContent = cachedVar match
    case None => readParseAndCache("Reading", firstReading = true)
    case Some(reference) => reference.get match
      case None => readParseAndCache("Re-reading evicted", firstReading = false)
      case Some(cached) => cached

  private def readParseAndCache(message: String, firstReading: Boolean): PageContent =
    val (frontMatter: FrontMatter, xml: Xml.Element) = markup.kind.readAndParse(
      site = page.site,
      sourcePath = sourcePath,
      standAloneFrontMatter = standAloneFrontMatter,
      message = message,
      firstReading = firstReading
    )
    
    cache(frontMatter, xml)

  def error(
    kind: PageError.Kind,
    message: String,
    cause: Option[Throwable] = None
  ): Unit = page.site.error(
    sourcePath,
    kind,
    message,
    cause
  )
