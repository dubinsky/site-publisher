package org.podval.tools.publish.page

import org.podval.tools.publish.util.{Files, Icon}
import com.microsoft.playwright.{Browser, BrowserType, Playwright, Page as PlaywrightPage}
import com.microsoft.playwright.options.{Margin, Media, WaitUntilState}
import scala.jdk.CollectionConverters.MapHasAsJava
import java.io.File

final class PdfPage(
  markupPage: FullMarkupPage
) extends RealPage(
  markupPage.site,
  markupPage.path.withExtension(PdfPage.extension)
):
  override def isDirectory: Boolean = false

  override def source: Option[PageSource] = None

  override def titleFromPath: String = path.fileName

  override protected def iconDefault: Icon = Icon.pdf

  // Write a PDF of an HTML page already written.
  // Note: written by Grok ;)
  override def write(): Unit =
    val htmlFile: File = markupPage.targetFile
    require(
      htmlFile.isFile,
      s"PDF source HTML not found (write the HTML page before the PDF): $htmlFile"
    )

    val page: PlaywrightPage = markupPage.site.browser.newPage()
    try
      // Match the printable content box so wrapping/pagination align with page.pdf margins.
      page.setViewportSize(PdfPage.pageSize.contentWidthPx, PdfPage.pageSize.contentHeightPx)
      page.emulateMedia(PlaywrightPage.EmulateMediaOptions().setMedia(Media.PRINT))
      page.navigate(
        markupPage.uri.toASCIIString,
        PlaywrightPage.NavigateOptions()
          .setWaitUntil(WaitUntilState.LOAD)
          .setTimeout(60_000)
      )
      // LOAD does not wait for webfonts (Google Fonts, Font Awesome, …).
      // document.fonts.ready resolves once faces used by the document have loaded (or failed).
      page.evaluate(
        "() => (document.fonts && document.fonts.ready) ? document.fonts.ready : Promise.resolve()"
      )
      // Reserve TOC leader/page-number columns so pagination matches the second print.
      page.evaluate(s"(() => { ${PdfPage.tocJs}; ensureTocLeaders(); })()")
      val probe: File = File.createTempFile("site-publisher-toc-", ".pdf")
      try
        page.pdf(PdfPage.pdfOptions(probe))
        page.evaluate(
          s"(pageById => { ${PdfPage.tocJs}; applyTocPageNumbers(pageById); })",
          PdfNamedDestinations.pageByName(probe).asJava
        )
        page.pdf(PdfPage.pdfOptions(targetFile))
        // Physical 1-based Arabic, same values as the TOC leaders just applied.
        PdfPageNumbers.stampOuterEdge(targetFile, PdfPage.pageSize)
      finally
        probe.delete()
    finally
      page.close()


// Arch (and other non-Ubuntu hosts) often trip Playwright's Debian-oriented
// dependency check even when headless Chromium works. Skip the check for the driver.
object PdfPage:
  val extension: String = "pdf"

  def playwright: Playwright = Playwright.create(
    Playwright.CreateOptions().setEnv(java.util.Map.of(
      "PLAYWRIGHT_SKIP_VALIDATE_HOST_REQUIREMENTS", "1"
    ))
  )

  def browser(playwright: Playwright): Browser = playwright.chromium.launch(
    BrowserType.LaunchOptions().setHeadless(true)
  )

  private lazy val tocJs: String =
    Files.readResource("/org/podval/tools/publish/page/fill-toc-page-numbers.js")

  private val pageSize: PdfPageSize = PdfPageSize.letter

  private def pdfOptions(to: File): PlaywrightPage.PdfOptions =
    PlaywrightPage.PdfOptions()
      .setPath(to.toPath)
      .setPrintBackground(true)
      .setWidth(s"${pageSize.widthIn}in")
      .setHeight(s"${pageSize.heightIn}in")
      .setMargin(Margin()
        .setTop(s"${pageSize.marginTopIn}in")
        .setRight(s"${pageSize.marginSideIn}in")
        .setBottom(s"${pageSize.marginBottomIn}in")
        .setLeft(s"${pageSize.marginSideIn}in")
      )
      // Folios are stamped by PdfPageNumbers after print. Chromium headers/footers
      // cannot switch left/right per page. Keep these margins so the probe and
      // final paginate the same way and so there is room for the outer-edge stamp.
      .setDisplayHeaderFooter(false)
