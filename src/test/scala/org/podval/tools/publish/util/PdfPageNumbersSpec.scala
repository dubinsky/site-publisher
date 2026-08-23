package org.podval.tools.publish.util

import org.apache.pdfbox.Loader
import org.apache.pdfbox.pdmodel.{PDDocument, PDPage, PDPageContentStream}
import org.apache.pdfbox.pdmodel.font.{PDType1Font, Standard14Fonts}
import org.apache.pdfbox.text.{PDFTextStripper, TextPosition}
import org.scalatest.funsuite.AnyFunSuite
import scala.jdk.CollectionConverters.ListHasAsScala
import java.io.File

final class PdfPageNumbersSpec extends AnyFunSuite:
  test("stampOuterEdge puts odd numbers on the right and even on the left") {
    val pdf: File = File.createTempFile("folios-", ".pdf")
    pdf.deleteOnExit()
    val font: PDType1Font = PDType1Font(Standard14Fonts.FontName.HELVETICA)
    val document = PDDocument()
    try
      for i <- 1 to 3 do
        val page = PDPage()
        document.addPage(page)
        val stream = PDPageContentStream(document, page)
        try
          stream.beginText()
          stream.setFont(font, 12f)
          stream.newLineAtOffset(100, 400)
          stream.showText(s"body-$i")
          stream.endText()
        finally
          stream.close()
      document.save(pdf)
    finally
      document.close()

    PdfPageNumbers.stampOuterEdge(pdf, PdfPageSize.letter, PdfFolioStyle.fallback)

    val stamped = Loader.loadPDF(pdf)
    try
      val pages = stamped.getPages
      assert(pages.getCount == 3)
      for i <- 0 until 3 do
        val pageNumber: Int = i + 1
        val page = pages.get(i)
        val glyphs: Seq[Glyph] = glyphsOn(stamped, pageNumber)
        assert(glyphs.exists(_.text == s"body-$pageNumber"), s"page $pageNumber lost body text")
        val folio: Glyph = glyphs.find(_.text == pageNumber.toString).getOrElse:
          fail(s"page $pageNumber has no folio; got ${glyphs.map(_.text)}")
        val mid: Float = page.getCropBox.getWidth / 2f
        if pageNumber % 2 == 0 then
          assert(folio.x < mid, s"even page $pageNumber folio x=${folio.x} should be left of $mid")
        else
          assert(folio.x > mid, s"odd page $pageNumber folio x=${folio.x} should be right of $mid")
        val fromBottom: Float = page.getCropBox.getHeight - folio.y
        assert(fromBottom < 72f, s"page $pageNumber folio not in the bottom inch (fromBottom=$fromBottom)")
    finally
      stamped.close()
  }

  test("stampOuterEdge uses the FolioStyle face and size from the page fonts") {
    val pdf: File = File.createTempFile("folios-style-", ".pdf")
    pdf.deleteOnExit()
    val font: PDType1Font = PDType1Font(Standard14Fonts.FontName.HELVETICA)
    val document = PDDocument()
    try
      val page = PDPage()
      document.addPage(page)
      val stream = PDPageContentStream(document, page)
      try
        stream.beginText()
        stream.setFont(font, 12f)
        stream.newLineAtOffset(100, 400)
        stream.showText("body-1")
        stream.endText()
      finally
        stream.close()
      document.save(pdf)
    finally
      document.close()

    val style: PdfFolioStyle = PdfFolioStyle(
      fontFamily = "Helvetica",
      fontSizePt = 18f,
      fontWeight = 400,
      italic = false,
      color = java.awt.Color(0x33, 0x33, 0x33)
    )
    PdfPageNumbers.stampOuterEdge(pdf, PdfPageSize.letter, style)

    val stamped = Loader.loadPDF(pdf)
    try
      val glyphs: Seq[Glyph] = glyphsOn(stamped, 1)
      val folio: Glyph = glyphs.find(_.text == "1").getOrElse:
        fail(s"no folio; got ${glyphs.map(_.text)}")
      assert(
        folio.fontName.contains("Helvetica"),
        s"expected Helvetica folio, got ${folio.fontName}"
      )
      assert(
        math.abs(folio.fontSizePt - 18f) < 0.5f,
        s"expected 18pt folio, got ${folio.fontSizePt}"
      )
    finally
      stamped.close()
  }

private final class GlyphStripper extends PDFTextStripper:
  var glyphs: Seq[Glyph] = Seq.empty
  setSortByPosition(true)
  override def writeString(text: String, textPositions: java.util.List[TextPosition]): Unit =
    if !textPositions.isEmpty then
      val first: TextPosition = textPositions.get(0)
      glyphs = glyphs :+ Glyph(
        textPositions.asScala.map(_.getUnicode).mkString,
        first.getXDirAdj,
        first.getYDirAdj,
        first.getFont.getName,
        first.getFontSizeInPt
      )

private final case class Glyph(text: String, x: Float, y: Float, fontName: String, fontSizePt: Float)

private def glyphsOn(document: PDDocument, pageNumber: Int): Seq[Glyph] =
  val stripper = GlyphStripper()
  stripper.setStartPage(pageNumber)
  stripper.setEndPage(pageNumber)
  stripper.getText(document)
  stripper.glyphs
