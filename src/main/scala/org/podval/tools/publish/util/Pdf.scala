package org.podval.tools.publish.util

import com.microsoft.playwright.{Browser, BrowserType, Playwright, Page as PlaywrightPage}
import com.microsoft.playwright.options.{Media, WaitUntilState}
import com.sun.net.httpserver.{HttpServer, SimpleFileServer}
import java.io.File
import java.net.{InetSocketAddress, URI}

// Note: written by Grok ;)
object Pdf:
  val extension: String = "pdf"

  // Letter at 96 CSS px / in — matches Playwright/Chromium default for format "Letter" with no margins.
  private val letterWidthPx: Int = Math.round(8.5 * 96).toInt   // 816
  private val letterHeightPx: Int = Math.round(11.0 * 96).toInt // 1056

  // Shared across all PDF pages for one generate() run; closed by close().
  private var playwright: Option[Playwright] = None
  private var browser: Option[Browser] = None
  private var server: Option[HttpServer] = None
  private var serverRoot: Option[File] = None
  private var serverPort: Option[Int] = None

  /** Shut down the shared HTTP server and Chromium/Playwright instance (no-op if never started). */
  def close(): Unit = synchronized:
    server.foreach(_.stop(0))
    server = None
    serverRoot = None
    serverPort = None
    browser.foreach(_.close())
    browser = None
    playwright.foreach(_.close())
    playwright = None

  private def sharedBrowser: Browser = synchronized:
    browser.getOrElse:
      // Arch (and other non-Ubuntu hosts) often trip Playwright's Debian-oriented
      // dependency check even when headless Chromium works. Skip the check for the driver.
      val pw: Playwright = Playwright.create(
        Playwright.CreateOptions().setEnv(java.util.Map.of(
          "PLAYWRIGHT_SKIP_VALIDATE_HOST_REQUIREMENTS", "1"
        ))
      )
      playwright = Some(pw)
      val launched: Browser = pw.chromium.launch(
        BrowserType.LaunchOptions().setHeadless(true)
      )
      browser = Some(launched)
      launched

  /**
   * Serve `siteRoot` over loopback HTTP (lazy, shared for one generate() run).
   * Root-relative URLs like `/assets/css/style.css` resolve correctly without rewriting HTML.
   */
  private def ensureServer(siteRoot: File): Int = synchronized:
    val root: File = siteRoot.getAbsoluteFile
    root.mkdirs()
    serverPort.filter(_ => serverRoot.contains(root)).getOrElse:
      server.foreach(_.stop(0))
      val httpServer: HttpServer = SimpleFileServer.createFileServer(
        InetSocketAddress("127.0.0.1", 0),
        root.toPath,
        SimpleFileServer.OutputLevel.NONE
      )
      httpServer.start()
      val port: Int = httpServer.getAddress.getPort
      server = Some(httpServer)
      serverRoot = Some(root)
      serverPort = Some(port)
      port

  private def pageUrl(port: Int, sitePath: String): String =
    val path: String = if sitePath.startsWith("/") then sitePath else s"/$sitePath"
    URI("http", null, "127.0.0.1", port, path, null, null).toASCIIString

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
    targetFile: File
  ): Unit =
    targetFile.getParentFile.mkdirs()

    val htmlFile: File = File(siteRoot, sitePath.stripPrefix("/"))
    require(
      htmlFile.isFile,
      s"PDF source HTML not found (write the HTML page before the PDF): $htmlFile"
    )

    val port: Int = ensureServer(siteRoot)
    val page: PlaywrightPage = sharedBrowser.newPage()
    try
      // Match Letter content width so wrapping/pagination aligns with page.pdf(format=Letter).
      page.setViewportSize(letterWidthPx, letterHeightPx)
      page.emulateMedia(PlaywrightPage.EmulateMediaOptions().setMedia(Media.PRINT))
      page.navigate(
        pageUrl(port, sitePath),
        PlaywrightPage.NavigateOptions()
          .setWaitUntil(WaitUntilState.LOAD)
          .setTimeout(60_000)
      )
      page.addStyleTag(PlaywrightPage.AddStyleTagOptions().setContent(tocPageNumberCss))
      page.evaluate(fillTocPageNumbersJs)
      page.pdf(
        PlaywrightPage.PdfOptions()
          .setPath(targetFile.toPath)
          .setPrintBackground(true)
          .setFormat("Letter")
      )
    finally
      page.close()

  /**
   * Dotted leaders + right-aligned page numbers for TOC entries (PDF only; applied in-page before print).
   *
   * Nested `ul`/`ol` normally get `margin-left` from site CSS (list indent). That shifts the whole nested
   * row — including page numbers — so subsection numbers no longer stack under section numbers.
   * Zero nested TOC list margins and indent titles only, so the page-number column stays aligned.
   */
  private val tocPageNumberCss: String =
    """
      |ul.toc > li.toc-section,
      |ul.toc > li.toc-section-selected {
      |  display: flex;
      |  flex-wrap: wrap;
      |  align-items: baseline;
      |  width: 100%;
      |  box-sizing: border-box;
      |}
      |ul.toc > li.toc-section > a,
      |ul.toc > li.toc-section-selected > a {
      |  flex: 0 1 auto;
      |  order: 1;
      |  min-width: 0;
      |}
      |ul.toc > li.toc-section > .toc-leader,
      |ul.toc > li.toc-section-selected > .toc-leader {
      |  flex: 1 1 auto;
      |  order: 2;
      |  border-bottom: 1px dotted rgba(0, 0, 0, 0.45);
      |  margin: 0 0.4em;
      |  min-width: 1em;
      |  height: 0.65em;
      |}
      |ul.toc > li.toc-section > .toc-page-number,
      |ul.toc > li.toc-section-selected > .toc-page-number {
      |  flex: 0 0 2.5em;
      |  order: 3;
      |  text-align: right;
      |  font-variant-numeric: tabular-nums;
      |}
      |/* Full-width nested TOC rows; cancel list margin so page numbers share one column. */
      |ul.toc > li.toc-section > ul.toc,
      |ul.toc > li.toc-section-selected > ul.toc {
      |  flex: 1 0 100%;
      |  order: 4;
      |  margin-left: 0;
      |  width: 100%;
      |  max-width: 100%;
      |  min-width: 0;
      |  box-sizing: border-box;
      |}
      |/* Indent nested titles only (not leaders / page numbers). */
      |ul.toc ul.toc > li.toc-section > a,
      |ul.toc ul.toc > li.toc-section-selected > a {
      |  padding-left: 1.5em;
      |}
      |ul.toc ul.toc ul.toc > li.toc-section > a,
      |ul.toc ul.toc ul.toc > li.toc-section-selected > a {
      |  padding-left: 3em;
      |}
      |ul.toc ul.toc ul.toc ul.toc > li.toc-section > a,
      |ul.toc ul.toc ul.toc ul.toc > li.toc-section-selected > a {
      |  padding-left: 4.5em;
      |}
      |""".stripMargin

  /**
   * For each TOC link to a fragment, append a dotted leader and the estimated PDF page number
   * of the target element (Letter page height, zero margins).
   * Runs twice so a large TOC that grows after numbers are injected still gets accurate pages.
   */
  private val fillTocPageNumbersJs: String =
    s"""
       |(() => {
       |  const pageHeight = $letterHeightPx;
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
