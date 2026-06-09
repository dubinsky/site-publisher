package org.podval.tools.publish.page

import org.podval.tools.publish.markup.Markup
import org.podval.tools.publish.page.Page
import org.podval.tools.publish.Path
import org.podval.xml.Xml
import scala.ref.SoftReference

final class PageSource(
  val page: Page,
  val markup: Markup,
  val sourcePath: Path,
  standAloneFrontMatter: Option[Path]
):
  private var cachedVar: Option[SoftReference[PageContent]] = None
  
  def cache(frontMatter: FrontMatter, xml: Xml.Element): PageContent =
    val result: PageContent = PageContent(
      source = this,
      frontMatter = frontMatter,
      xmlVar = xml,
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
    