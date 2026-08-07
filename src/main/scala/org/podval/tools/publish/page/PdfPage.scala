package org.podval.tools.publish.page

import org.podval.tools.publish.util.{Icon, Pdf}

final class PdfPage(
  markupPage: FullMarkupPage
) extends RealPage(
  markupPage.site,
  markupPage.path.withExtension(Pdf.extension)
):
  override def isDirectory: Boolean = false

  override def source: Option[PageSource] = None

  override def titleFromPath: String = path.fileName + path.extensionString

  override protected def iconDefault: Icon = Icon.pdf

  override def write(): Unit = Pdf.renderPdf(
    htmlRaw = markupPage.textContent,
    siteRoot = markupPage.site.targetDirectory,
    targetFile = targetFile
  )
