package org.podval.tools.publish.page

import org.apache.pdfbox.Loader
import org.apache.pdfbox.pdmodel.{PDDocument, PDPage, PDPageContentStream, PDPageTree}
import org.apache.pdfbox.pdmodel.PDPageContentStream.AppendMode
import org.apache.pdfbox.pdmodel.font.{PDType1Font, Standard14Fonts}
import java.awt.Color
import java.io.File
import java.nio.file.{Files as NFiles, StandardCopyOption}

/**
 * Outer-edge folios after Chromium print. Odd (recto) on the right, even (verso)
 * on the left. Numbers are physical 1-based Arabic, the same sequence
 * `PdfNamedDestinations.pageByName` feeds `applyTocPageNumbers`.
 *
 * Traditional book practice would instead: pick a first-body-page index; stamp
 * lowercase roman on shown front-matter pages and leave title / colophon /
 * dedication blind; restart Arabic at 1 on the first body recto; and pass those
 * display numbers into `applyTocPageNumbers` instead of PDF page indices.
 * Chromium cannot do `:left`/`:right` or mixed numbering, so any of that still
 * belongs in this pass.
 *
 * Note: written by Grok ;)
 */
object PdfPageNumbers:
  def stampOuterEdge(pdf: File, pageSize: PdfPageSize): Unit =
    val tmp: File = File.createTempFile("site-publisher-folios-", ".pdf", pdf.getParentFile)
    try
      val document: PDDocument = Loader.loadPDF(pdf)
      try
        val font: PDType1Font = PDType1Font(Standard14Fonts.FontName.TIMES_ROMAN)
        val pages: PDPageTree = document.getPages
        for i <- 0 until pages.getCount do stampPage(document, pages.get(i), i + 1, pageSize, font)
        document.save(tmp)
      finally
        document.close()
      NFiles.move(tmp.toPath, pdf.toPath, StandardCopyOption.REPLACE_EXISTING)
    finally
      if tmp.exists then tmp.delete()

  private def stampPage(
    document: PDDocument,
    page: PDPage,
    number: Int,
    pageSize: PdfPageSize,
    font: PDType1Font
  ): Unit =
    val box = page.getCropBox
    val text: String = number.toString
    val textWidth: Float = font.getStringWidth(text) / 1000f * fontSize
    val x: Float =
      if number % 2 == 0
      then box.getLowerLeftX + pageSize.sideInset
      else box.getLowerLeftX + box.getWidth - pageSize.sideInset - textWidth
    val y: Float = box.getLowerLeftY + pageSize.baselineFromBottom
    val stream: PDPageContentStream =
      PDPageContentStream(document, page, AppendMode.APPEND, true, true)
    try
      stream.beginText()
      stream.setFont(font, fontSize)
      stream.setNonStrokingColor(ink)
      stream.newLineAtOffset(x, y)
      stream.showText(text)
      stream.endText()
    finally
      stream.close()

  private val fontSize: Float = 10f
  private val ink: Color = Color(0x33, 0x33, 0x33)
