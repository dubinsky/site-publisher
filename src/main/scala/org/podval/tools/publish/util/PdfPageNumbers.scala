package org.podval.tools.publish.util

import org.apache.pdfbox.Loader
import org.apache.pdfbox.pdmodel.{PDDocument, PDPage, PDPageContentStream, PDPageTree}
import org.apache.pdfbox.pdmodel.PDPageContentStream.AppendMode
import org.apache.pdfbox.pdmodel.font.{PDFont, PDType1Font, Standard14Fonts}
import scala.jdk.CollectionConverters.IterableHasAsScala
import java.io.{File, IOException}
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
  def stampOuterEdge(pdf: File, pageSize: PdfPageSize, style: PdfFolioStyle): Unit =
    val tmp: File = File.createTempFile("site-publisher-folios-", ".pdf", pdf.getParentFile)
    try
      val document: PDDocument = Loader.loadPDF(pdf)
      try
        val pages: PDPageTree = document.getPages
        for i <- 0 until pages.getCount do stampPage(document, pages.get(i), i + 1, pageSize, style)
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
    style: PdfFolioStyle
  ): Unit =
    val font: PDFont = fontFor(page, style)
    val box = page.getCropBox
    val text: String = number.toString
    val textWidth: Float = font.getStringWidth(text) / 1000f * style.fontSizePt
    val x: Float =
      if number % 2 == 0
      then box.getLowerLeftX + pageSize.sideInset
      else box.getLowerLeftX + box.getWidth - pageSize.sideInset - textWidth
    val y: Float = box.getLowerLeftY + pageSize.baselineFromBottom
    val stream: PDPageContentStream = PDPageContentStream(document, page, AppendMode.APPEND, true, true)
    try
      stream.beginText()
      stream.setFont(font, style.fontSizePt)
      stream.setNonStrokingColor(style.color)
      stream.newLineAtOffset(x, y)
      stream.showText(text)
      stream.endText()
    finally
      stream.close()

  private val genericFamilies: Set[String] = Set(
    "serif", "sans-serif", "monospace", "cursive", "fantasy",
    "system-ui", "ui-serif", "ui-sans-serif", "ui-monospace",
    "-apple-system", "blinkmacsystemfont"
  )

  private val familySuffixes: Seq[String] = Seq(
    "", "regular", "roman", "medium", "light",
    "bold", "italic", "oblique", "bolditalic", "boldoblique"
  )

  private def fontFor(page: PDPage, style: PdfFolioStyle): PDFont =
    val available: Seq[PDFont] = fontsOn(page)
    cssFamilies(style.fontFamily)
      .filterNot(family => genericFamilies.contains(family.toLowerCase))
      .view
      .flatMap: family =>
        available.find(font => familyMatches(font, family) && weightAndStyleMatch(font, style))
      .find(font => canEncodeDigits(font))
      .getOrElse(fallback)

  private def fallback: PDFont = PDType1Font(Standard14Fonts.FontName.TIMES_ROMAN)

  private def fontsOn(page: PDPage): Seq[PDFont] =
    Option(page.getResources).toSeq.flatMap: resources =>
      resources.getFontNames.asScala.toSeq.flatMap: name =>
        try Option(resources.getFont(name))
        catch case _: IOException => None

  private def cssFamilies(stack: String): Seq[String] =
    stack.split(',').toSeq
      .map(_.trim.replaceAll("^['\"]|['\"]$", ""))
      .filter(_.nonEmpty)

  private def familyMatches(font: PDFont, cssFamily: String): Boolean =
    val want: String = compact(cssFamily)
    fontNames(font).exists: raw =>
      val got: String = compact(raw)
      familySuffixes.exists(suffix => got == want + suffix)

  private def fontNames(font: PDFont): Seq[String] =
    val desc = Option(font.getFontDescriptor)
    Seq(
      Option(font.getName),
      desc.flatMap(d => Option(d.getFontName)),
      desc.flatMap(d => Option(d.getFontFamily))
    ).flatten

  private def weightAndStyleMatch(font: PDFont, style: PdfFolioStyle): Boolean =
    val desc = Option(font.getFontDescriptor)
    val compacted: String = fontNames(font).map(compact).mkString
    val italic: Boolean = desc.exists(_.isItalic) || compacted.contains("italic") || compacted.contains("oblique")
    if italic != style.italic then false else
      val weight: Float = desc.map(_.getFontWeight).getOrElse(0f)
      if weight == 0
      then (style.fontWeight >= 600) == compacted.contains("bold")
      else math.abs(weight - style.fontWeight) < 150

  private def canEncodeDigits(font: PDFont): Boolean =
    try
      font.getStringWidth("0123456789")
      true
    catch case _: IOException => false

  private def compact(name: String): String =
    name.replaceFirst("^[A-Za-z]{6}\\+", "").toLowerCase.replaceAll("[^a-z0-9]", "")
