package org.podval.tools.publish.util

import com.microsoft.playwright.{Browser, BrowserType, Playwright, Page as PlaywrightPage}
import com.microsoft.playwright.options.{Media, WaitUntilState}
import java.io.File
import scala.util.matching.Regex

// Note: written by Grok ;)
// TODO
// - suppress navigation and footer for print media in CSS;
// - page numbers are staggered!
object Pdf:
  val extension: String = "pdf"

  // Letter at 96 CSS px / in — matches Playwright/Chromium default for format "Letter" with no margins.
  private val letterWidthPx: Int = Math.round(8.5 * 96).toInt   // 816
  private val letterHeightPx: Int = Math.round(11.0 * 96).toInt // 1056

  // Shared across all PDF pages for one generate() run; closed by close().
  private var playwright: Option[Playwright] = None
  private var browser: Option[Browser] = None

  /** Shut down the shared Chromium/Playwright instance (no-op if never started). */
  def close(): Unit = synchronized:
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

  def renderPdf(
    htmlRaw: String,
    siteRoot: File,
    targetFile: File
  ): Unit =
    targetFile.getParentFile.mkdirs()

    val html = prepareHtml(
      html = htmlRaw,
      siteRoot = siteRoot
    )
    // Chromium blocks file:// (and thus local CSS/images) from setContent/about:blank.
    // Navigate a real file under the site root so stylesheet @imports and assets load.
    siteRoot.mkdirs()
    // TODO instead of writing the content...
    val tempHtml: File = File.createTempFile("pdf-render-", ".html", siteRoot)
    try
      Files.write(tempHtml, html)
      val page: PlaywrightPage = sharedBrowser.newPage()
      try
        // Match Letter content width so wrapping/pagination aligns with page.pdf(format=Letter).
        page.setViewportSize(letterWidthPx, letterHeightPx)
        page.emulateMedia(PlaywrightPage.EmulateMediaOptions().setMedia(Media.PRINT))
        page.navigate(
          tempHtml.getAbsoluteFile.toPath.toUri.toString,
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
    finally
      tempHtml.delete()

  // Root-relative site URLs (`/assets/...`), excluding protocol-relative `//...`.
  private val rootRelativeUrl: Regex = """(?i)(\b(?:href|src)=")(/(?!/)[^"]*)(")""".r

  /**
   * Prepare HTML for Chromium file:// navigation:
   * - inject `<base href="file:.../_site/">` so relative asset URLs resolve under the site root
   * - rewrite root-relative `/assets/...` links to relative `assets/...` (absolute `/...` ignores `<base>`)
   */
  private def prepareHtml(html: String, siteRoot: File): String =
    val baseHref: String = siteRoot.getAbsoluteFile.toPath.toUri.toString
    val withBase: String = html.replaceFirst(
      """(?i)<head(\s[^>]*)?>""",
      s"""<head$$1><base href="$baseHref">"""
    )
    rootRelativeUrl.replaceAllIn(withBase, m =>
      m.group(1) + m.group(2).stripPrefix("/") + m.group(3)
    )

  /** Dotted leaders + right-aligned page numbers for TOC entries (PDF only; applied in-page before print). */
  private val tocPageNumberCss: String =
    """
      |ul.toc > li.toc-section,
      |ul.toc > li.toc-section-selected {
      |  display: flex;
      |  flex-wrap: wrap;
      |  align-items: baseline;
      |}
      |ul.toc > li.toc-section > a,
      |ul.toc > li.toc-section-selected > a {
      |  flex: 0 1 auto;
      |  order: 1;
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
      |  flex: 0 0 auto;
      |  order: 3;
      |  font-variant-numeric: tabular-nums;
      |}
      |ul.toc > li.toc-section > ul.toc,
      |ul.toc > li.toc-section-selected > ul.toc {
      |  flex: 1 0 100%;
      |  order: 4;
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
