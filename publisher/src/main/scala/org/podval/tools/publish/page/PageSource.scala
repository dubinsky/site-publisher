package org.podval.tools.publish.page

import org.podval.metadata.Language
import org.podval.tools.publish.markup.Markup
import org.podval.tools.publish.site.{PageError, PageErrorReporter, Path}
import org.podval.xml.Xml
import scala.ref.SoftReference

final class PageSource(
  val page: MarkupPage,
  val markup: Markup,
  val sourcePath: Path,
  val frontMatterStandAlone: Option[Path]
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

  override def languageSpec: Language.Spec = page.site.languageSpec

  override def teiDefaultCalendarIsJulian: Boolean = page.site.teiDefaultCalendarIsJulian

  private var documentCodeVar: Option[String] = None

  /** Set before TEI conversion. Does not read `page.lang` (that would re-enter content). */
  def setDocumentCode(code: String): Unit = documentCodeVar = Some(code)

  override def interfaceChrome: Option[(String, String)] =
    documentCodeVar.filter(_ != interfaceCode).map(_ => (interfaceCode, interfaceDir))

  private def interfaceCode: String = page.site.languageSpec.language.get.name

  private def interfaceDir: String =
    if interfaceCode == "he" || interfaceCode == "yi" then "rtl" else "ltr"

  private var contentVar: Option[SoftReference[PageContent]] = None

  def cache(
    frontMatter: FrontMatter,
    xml: Xml.Element,
    firstReading: Boolean = true
  ): PageContent =
    val result: PageContent = PageContent(this, frontMatter, xml, firstReading)
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

      cache(frontMatter, xml, firstReading)
    
    contentVar match
      case None => readParseAndCache("Reading", firstReading = true)
      case Some(reference) => reference.get match
        case None => readParseAndCache("Re-reading evicted", firstReading = false)
        case Some(cached) => cached

  /** Source element before `process`. The published TEI is stamped from this, not from the HTML tree. */
  def readRaw(): Xml.Element =
    markup.readAndParse(
      site = page.site,
      sourcePath = sourcePath,
      frontMatterStandAlone = frontMatterStandAlone,
      message = "Reading TEI to publish",
      firstReading = false
    )._2
