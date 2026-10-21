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
  val frontMatterStandAlone: Option[Path],
  val recordParseErrors: Boolean = true
) extends PageErrorReporter:
  // A nested `content` read must not clear this before the outer parse finishes.
  private var quietDepth: Int = 0

  override def error(
    kind: PageError.Kind,
    message: String,
    cause: Option[Throwable] = None
  ): Unit =
    if quietDepth > 0 then ()
    else page.site.error(
      sourcePath,
      kind,
      message,
      cause
    )

  override def languageSpec: Language.Spec = page.site.languageSpec

  override def teiDefaultCalendarIsJulian: Boolean = page.site.teiDefaultCalendarIsJulian

  private var documentCodeVar: Option[String] = None

  /** Stored before conversion so `lang` and the chrome comparison share one code. */
  def setDocumentCode(code: String): Unit = documentCodeVar = Some(code)

  def documentCode: Option[String] = documentCodeVar

  /** Document code before the site fallback. Absent when the file declares nothing. */
  private var fileDocumentCodeVar: Option[String] = None

  def setFileDocumentCode(code: Option[String]): Unit = fileDocumentCodeVar = code

  def fileDocumentCode: Option[String] = fileDocumentCodeVar

  private var languageDisagreementVar: Boolean = false

  /** The file's own signals already disagree. The reading check must not add a second error. */
  def languageDisagrees: Boolean = languageDisagreementVar

  def markLanguageDisagreement(): Unit = languageDisagreementVar = true

  override def interfaceChrome: Option[(String, String)] =
    documentCode.filter(_ != page.interfaceCode).map(_ => (page.interfaceCode, page.interfaceDir))

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
    val quiet: Boolean = !recordParseErrors
    if quiet then quietDepth += 1
    try loadContent()
    finally if quiet then quietDepth -= 1

  private def loadContent(): PageContent =
    def readParseAndCache(message: String, firstReading: Boolean): PageContent =
      val (frontMatter: FrontMatter, xml: Xml.Element) = markup.readAndParse(
        site = page.site,
        sourcePath = sourcePath,
        frontMatterStandAlone = frontMatterStandAlone,
        message = message,
        firstReading = firstReading,
        recordErrors = recordParseErrors
      )

      cache(frontMatter, xml, firstReading)

    contentVar match
      case None => readParseAndCache("Reading", firstReading = recordParseErrors)
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
