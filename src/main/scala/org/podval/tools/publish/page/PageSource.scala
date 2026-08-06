package org.podval.tools.publish.page

import org.podval.tools.publish.markup.Markup
import org.podval.tools.publish.site.{PageError, PageErrorReporter, Path}
import org.podval.xml.Xml
import scala.ref.SoftReference

final class PageSource(
  val page: FullMarkupPage,
  val markup: Markup,
  val sourcePath: Path,
  frontMatterStandAlone: Option[Path]
) extends PageErrorReporter:
  
  override def error(
    kind: PageError.Kind,
    message: String,
    cause: Option[Throwable] = None
  ): Unit = page.site.error(
    sourcePath,
    kind,
    message,
    cause
  )

  private var contentVar: Option[SoftReference[PageContent]] = None

  def cache(frontMatter: FrontMatter, xml: Xml.Element): PageContent =
    val result: PageContent = PageContent(this, frontMatter, xml)
    contentVar = Some(SoftReference(result))
    result

  def content: PageContent =
    def readParseAndCache(message: String, firstReading: Boolean): PageContent =
      val (frontMatter: FrontMatter, xml: Xml.Element) = markup.readAndParse(
        site = page.site,
        sourcePath = sourcePath,
        frontMatterStandAlone = frontMatterStandAlone,
        message = message,
        firstReading = firstReading
      )

      cache(frontMatter, xml)
    
    contentVar match
      case None => readParseAndCache("Reading", firstReading = true)
      case Some(reference) => reference.get match
        case None => readParseAndCache("Re-reading evicted", firstReading = false)
        case Some(cached) => cached
