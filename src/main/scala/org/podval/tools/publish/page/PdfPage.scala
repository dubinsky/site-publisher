package org.podval.tools.publish.page

import org.podval.tools.publish.util.Icon
import com.microsoft.playwright.{Browser, BrowserType, Playwright}
import com.microsoft.playwright.Page as PlaywrightPage
import com.microsoft.playwright.options.WaitUntilState
import java.io.File
import scala.util.matching.Regex

// Note: written by Grok ;)
object PdfPage:
  val extension: String = "pdf"

  // Root-relative site URLs (`/assets/...`), excluding protocol-relative `//...`.
  private val rootRelativeUrl: Regex = """(?i)(\b(?:href|src)=")(/(?!/)[^"]*)(")""".r

  /**
   * Prepare HTML for Chromium `setContent`:
   * - inject `<base href="file:.../_site/">` so relative asset URLs resolve under the site root
   * - rewrite root-relative `/assets/...` links to relative `assets/...` (absolute `/...` ignores `<base>`)
   */
  private[page] def prepareHtml(html: String, siteRoot: File): String =
    val baseHref: String = siteRoot.getAbsoluteFile.toPath.toUri.toString
    val withBase: String = html.replaceFirst(
      """(?i)<head(\s[^>]*)?>""",
      s"""<head$$1><base href="$baseHref">"""
    )
    rootRelativeUrl.replaceAllIn(withBase, m =>
      m.group(1) + m.group(2).stripPrefix("/") + m.group(3)
    )

final class PdfPage(
  markupPage: FullMarkupPage
) extends RealPage(
  markupPage.site,
  markupPage.path.withExtension(PdfPage.extension)
):
  override def isDirectory: Boolean = false

  override def titleFromPath: String = path.fileName + path.extensionString

  override protected def iconDefault: Icon = Icon.pdf

  override def source: Option[PageSource] = None

  override def write(): Unit =
    targetFile.getParentFile.mkdirs()

    val html: String = PdfPage.prepareHtml(
      html = markupPage.textContent,
      siteRoot = site.targetDirectory
    )

    val playwright: Playwright = Playwright.create()
    try
      val browser: Browser = playwright.chromium.launch(
        BrowserType.LaunchOptions().setHeadless(true)
      )
      try
        val page: PlaywrightPage = browser.newPage()
        page.setContent(
          html,
          PlaywrightPage.SetContentOptions()
            .setWaitUntil(WaitUntilState.LOAD)
            .setTimeout(60_000)
        )
        page.pdf(
          PlaywrightPage.PdfOptions()
            .setPath(targetFile.toPath)
            .setPrintBackground(true)
            .setFormat("Letter")
        )
      finally
        browser.close()
    finally
      playwright.close()
