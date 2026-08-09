package org.podval.tools.publish.util

import com.microsoft.playwright.{Browser, BrowserType, Playwright, Page as PlaywrightPage}
import com.microsoft.playwright.options.{Margin, Media, WaitUntilState}
import com.sun.net.httpserver.{HttpServer, SimpleFileServer}
import java.io.File
import java.net.{InetSocketAddress, URI}

// Note: written by Grok ;)
object Pdf:
  val extension: String = "pdf"

  // Arch (and other non-Ubuntu hosts) often trip Playwright's Debian-oriented
  // dependency check even when headless Chromium works. Skip the check for the driver.

  def playwright: Playwright = Playwright.create(
    Playwright.CreateOptions().setEnv(java.util.Map.of(
      "PLAYWRIGHT_SKIP_VALIDATE_HOST_REQUIREMENTS", "1"
    ))
  )

  def browser(playwright: Playwright): Browser = playwright.chromium.launch(
    BrowserType.LaunchOptions().setHeadless(true)
  )

  def httpServer(siteRoot: File): HttpServer =
    val result: HttpServer = SimpleFileServer.createFileServer(
      InetSocketAddress("127.0.0.1", 0),
      siteRoot.getAbsoluteFile.toPath,
      SimpleFileServer.OutputLevel.NONE
    )
    result.start()
    result

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
    Math.round((letterWidthIn - 2 * marginIn) * cssPxPerIn).toInt   // 720
  private val contentHeightPx: Int =
    Math.round((letterHeightIn - 2 * marginIn) * cssPxPerIn).toInt  // 960

  /**
   * Render a PDF of a page already written under `siteRoot`.
   *
   * @param sitePath site-absolute path of the HTML page (e.g. `/asciidoc/calendar/calendar.html`)
   * @param siteRoot generated site directory (`_site`)
   * @param targetFile where to write the PDF
   */
  def renderPdf(
    sitePath: String,
    siteRoot: File,
    targetFile: File,
    httpServer: HttpServer,
    browser: Browser
  ): Unit =
    val htmlFile: File = File(siteRoot, sitePath.stripPrefix("/"))
    require(
      htmlFile.isFile,
      s"PDF source HTML not found (write the HTML page before the PDF): $htmlFile"
    )

    val port: Int = httpServer.getAddress.getPort
    val path: String = if sitePath.startsWith("/") then sitePath else s"/$sitePath"
    val url: String = URI("http", null, "127.0.0.1", port, path, null, null).toASCIIString

    val page: PlaywrightPage = browser.newPage()
    try
      // Match the printable content box so wrapping/pagination align with page.pdf margins.
      page.setViewportSize(contentWidthPx, contentHeightPx)
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
      page.evaluate(fillTocPageNumbersJs)
      page.pdf(
        PlaywrightPage.PdfOptions()
          .setPath(targetFile.toPath)
          .setPrintBackground(true)
          .setFormat("Letter")
          .setMargin(pdfMargin)
      )
    finally
      page.close()

  /**
   * For each TOC link to a fragment, append a dotted leader and the estimated PDF page number
   * of the target element (printable content height = Letter minus paper margins).
   * Runs twice so a large TOC that grows after numbers are injected still gets accurate pages.
   * Print styling for the injected nodes lives in layout.css (@media print).
   */
  private val fillTocPageNumbersJs: String =
    s"""
       |(() => {
       |  const pageHeight = $contentHeightPx;
       |  const fill = () => {
       |    document.querySelectorAll('ul.toc a[href*="#"]').forEach((anchor) => {
       |      const href = anchor.getAttribute('href') || '';
       |      const hash = href.indexOf('#');
       |      if (hash < 0) return;
       |      const id = decodeURIComponent(href.substring(hash + 1));
       |      if (!id) return;
       |      const target = document.getElementById(id);
       |      if (!target) return;
       |      const top = target.getBoundingClientRect().top + window.scrollY;
       |      const pageNumber = Math.max(1, Math.floor(top / pageHeight) + 1);
       |      const li = anchor.parentElement;
       |      if (!li) return;
       |      let leader = li.querySelector(':scope > .toc-leader');
       |      if (!leader) {
       |        leader = document.createElement('span');
       |        leader.className = 'toc-leader';
       |        leader.setAttribute('aria-hidden', 'true');
       |        li.insertBefore(leader, anchor.nextSibling);
       |      }
       |      let pageEl = li.querySelector(':scope > .toc-page-number');
       |      if (!pageEl) {
       |        pageEl = document.createElement('span');
       |        pageEl.className = 'toc-page-number';
       |        li.insertBefore(pageEl, leader.nextSibling);
       |      }
       |      pageEl.textContent = String(pageNumber);
       |    });
       |  };
       |  fill();
       |  fill();
       |})()
       |""".stripMargin
