package org.podval.tools.publish.page

import org.podval.tools.publish.site.Site
import org.podval.tools.publish.util.{Files, Icon}
import com.microsoft.playwright.{Browser, BrowserType, Playwright, Page as PlaywrightPage}
import com.microsoft.playwright.options.{Margin, Media, WaitUntilState}
import com.sun.net.httpserver.{HttpServer, SimpleFileServer}
import java.io.File
import java.net.{InetSocketAddress, URI}

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
  override def write(): Unit =
    val htmlFile: File = markupPage.targetFile
    require(
      htmlFile.isFile,
      s"PDF source HTML not found (write the HTML page before the PDF): $htmlFile"
    )

    val url: String = URI(
      "http",
      null,
      PdfPage.localhost,
      markupPage.site.httpServer.getAddress.getPort,
      markupPage.path.toString,
      null,
      null
    ).toASCIIString

    val page: PlaywrightPage = markupPage.site.browser.newPage()
    try
      // Match the printable content box so wrapping/pagination align with page.pdf margins.
      page.setViewportSize(PdfPage.contentWidthPx, PdfPage.contentHeightPx)
      page.emulateMedia(PlaywrightPage.EmulateMediaOptions().setMedia(Media.PRINT))
      page.navigate(
        url,
        PlaywrightPage.NavigateOptions()
          .setWaitUntil(WaitUntilState.LOAD)
          .setTimeout(60_000)
      )
      // LOAD does not wait for webfonts (Google Fonts, Font Awesome, …).
      // document.fonts.ready resolves once faces used by the document have loaded (or failed).
      page.evaluate(
        """() => (document.fonts && document.fonts.ready)
          |  ? document.fonts.ready
          |  : Promise.resolve()""".stripMargin
      )
      // Inject TOC leaders/page numbers; print styling is in layout.css (@media print).
      // Parenthesize: resource is a function declaration; evaluate needs a function expression.
      page.evaluate(s"(${PdfPage.fillTocPageNumbersJs})", Int.box(PdfPage.contentHeightPx))
      page.pdf(
        PlaywrightPage.PdfOptions()
          .setPath(targetFile.toPath)
          .setPrintBackground(true)
          .setFormat("Letter")
          .setMargin(PdfPage.pdfMargin)
      )
    finally
      page.close()


// Note: written by Grok ;)
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

  private val localhost: String = "127.0.0.1"

  def httpServer(site: Site): HttpServer =
    val result: HttpServer = SimpleFileServer.createFileServer(
      InetSocketAddress(localhost, 0),
      site.targetDirectory.getAbsoluteFile.toPath,
      SimpleFileServer.OutputLevel.NONE
    )
    result.start()
    result

  // TOC script (function declaration); arg is printable content height in CSS px.
  private lazy val fillTocPageNumbersJs: String =
    Files.readResource("/org/podval/tools/publish/page/fill-toc-page-numbers.js")

  // Letter paper at 96 CSS px / in (Playwright/Chromium default).
  private val letterWidthIn: Double = 8.5
  private val letterHeightIn: Double = 11.0
  private val cssPxPerIn: Double = 96.0

  // Paper margins for page.pdf (CSS length strings accepted by Chromium).
  private val marginIn: Double = 0.5
  private val marginCss: String = s"${marginIn}in"
  private val pdfMargin: Margin = Margin()
    .setTop(marginCss)
    .setRight(marginCss)
    .setBottom(marginCss)
    .setLeft(marginCss)

  // Content box inside margins — viewport and TOC page math must match this, not full paper.
  private val contentWidthPx: Int =
    Math.round((letterWidthIn - 2 * marginIn) * cssPxPerIn).toInt // 720
  private val contentHeightPx: Int =
    Math.round((letterHeightIn - 2 * marginIn) * cssPxPerIn).toInt // 960
