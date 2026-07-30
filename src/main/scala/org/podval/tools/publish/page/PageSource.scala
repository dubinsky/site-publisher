package org.podval.tools.publish.page

import org.podval.tools.publish.markup.Markup
import org.podval.tools.publish.site.{PageError, Path}
import org.podval.xml.{Xml, XmlDialect}
import scala.ref.SoftReference

final class PageSource(
  val page: OriginalMarkupPage,
  val markup: Markup,
  val sourcePath: Path,
  standAloneFrontMatter: Option[Path]
):
  def xmlDialect: XmlDialect = markup.xmlDialect

  private var cachedVar: Option[SoftReference[PageContent]] = None
  
  def cache(frontMatter: FrontMatter, xml: Xml.Element): PageContent =
    val (xmlProcessed: Xml.Element, title: Option[Xml.Element]) =
      markup.retrieveTitle(markup.process(this, xml))

    // TODO error if both front matter and content titles are present.
    
    val result: PageContent = PageContent(
      source = this,
      frontMatter = frontMatter,
      title = title,
      xml = xmlProcessed,
    )

    cachedVar = Some(SoftReference(result))

    result
  
  def content: PageContent = cachedVar match
    case None => readParseAndCache("Reading", firstReading = true)
    case Some(reference) => reference.get match
      case None => readParseAndCache("Re-reading evicted", firstReading = false)
      case Some(cached) => cached

  private def readParseAndCache(message: String, firstReading: Boolean): PageContent =
    val (frontMatter: FrontMatter, xml: Xml.Element) = markup.readAndParse(
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
