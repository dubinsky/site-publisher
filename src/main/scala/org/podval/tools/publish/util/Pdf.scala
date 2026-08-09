package org.podval.tools.publish.util

import com.microsoft.playwright.{Browser, BrowserType, Playwright, Page as PlaywrightPage}
import com.microsoft.playwright.options.{Margin, Media, WaitUntilState}
import org.podval.tools.publish.site.Site
import java.io.File
import java.net.URI

// Note: written by Grok ;)
object Pdf:
  val extension: String = "pdf"

  // Letter paper at 96 CSS px / in (Playwright/Chromium default).
  private val letterWidthIn: Double = 8.5
  private val letterHeightIn: Double = 11.0
  private val cssPxPerIn: Double = 96.0

  // Paper margins for page.pdf (CSS length strings accepted by Chromium).
  private val marginIn: Double = 0.5
  private val marginCss: String = s"${marginIn}in"
  private val pdfMargin: Margin =
    Margin()
      .setTop(marginCss)
      .setRight(marginCss)
      .setBottom(marginCss)
      .setLeft(marginCss)

  // Content box inside margins — viewport and TOC page math must match this, not full paper.
  private val contentWidthPx: Int =
    Math.round((letterWidthIn - 2 * marginIn) * cssPxPerIn).toInt   // 720
  private val contentHeightPx: Int =
    Math.round((letterHeightIn - 2 * marginIn) * cssPxPerIn).toInt  // 960

  // Shared across all PDF pages for one generate() run; closed by close().
  private var playwright: Option[Playwright] = None

  /** Shut down the shared HTTP server and Chromium/Playwright instance (no-op if never started). */
  def close(): Unit = synchronized:
    playwright.foreach(_.close())
    playwright = None
  
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
    site: Site,
    sitePath: String,
    siteRoot: File,
    targetFile: File,
    browser: Browser
  ): Unit =
    targetFile.getParentFile.mkdirs()

    val htmlFile: File = File(siteRoot, sitePath.stripPrefix("/"))
    require(
      htmlFile.isFile,
      s"PDF source HTML not found (write the HTML page before the PDF): $htmlFile"
    )

    val port: Int = site.httpServer.getAddress.getPort
    val page: PlaywrightPage = browser.newPage()
    try
      // Match the printable content box so wrapping/pagination align with page.pdf margins.
      page.setViewportSize(contentWidthPx, contentHeightPx)
      page.emulateMedia(PlaywrightPage.EmulateMediaOptions().setMedia(Media.PRINT))
      page.navigate(
        pageUrl(port, sitePath),
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
      page.addStyleTag(PlaywrightPage.AddStyleTagOptions().setContent(tocPageNumberCss))
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
   * of the target element (printable content height = Letter minus paper margins).
   * Runs twice so a large TOC that grows after numbers are injected still gets accurate pages.
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
