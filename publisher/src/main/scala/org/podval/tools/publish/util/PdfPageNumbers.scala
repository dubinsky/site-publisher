package org.podval.tools.publish.util

import org.apache.pdfbox.Loader
import org.apache.pdfbox.pdmodel.{PDDocument, PDPage, PDPageContentStream, PDPageTree}
import org.apache.pdfbox.pdmodel.PDPageContentStream.AppendMode
import org.apache.pdfbox.pdmodel.font.{PDFont, PDType0Font, PDType1Font, PDType3Font}
import org.apache.pdfbox.pdmodel.font.Standard14Fonts.FontName
import scala.jdk.CollectionConverters.{IterableHasAsScala, IteratorHasAsScala}
import scala.util.control.NonFatal
import java.io.{ByteArrayInputStream, File, IOException}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files as NFiles, StandardCopyOption}
import java.util.concurrent.TimeUnit
import java.util.Locale

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
        if pages.getCount > 0 then
          val font: PDFont = fontFor(document, pages.get(0), style)
          for i <- 0 until pages.getCount do stampPage(document, pages.get(i), i + 1, pageSize, style, font)
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
    style: PdfFolioStyle,
    font: PDFont
  ): Unit =
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

  // Chromium/Skia embeds WOFF2 (Google Fonts) as Type3; PDFBox cannot encode into that
  // and there is no Chromium flag to dump TTF instead. Load a TrueType of the CSS family:
  // Google Fonts CSS with a wget UA (same host as the page, works in CI with no system
  // fonts), else a system TTF/OTF. Then a page font that can encode, then Standard 14.
  private def fontFor(document: PDDocument, page: PDPage, style: PdfFolioStyle): PDFont =
    loadedType0(document, style)
      .orElse(pageFont(page, style))
      .orElse(standard14(style))
      .getOrElse(PDType1Font(FontName.TIMES_ROMAN))

  /** First TTF/OTF for the CSS stack: Google Fonts, else this machine. */
  private[util] def fontBytesFor(style: PdfFolioStyle): Option[Array[Byte]] =
    namedFamilies(style).view.flatMap: family =>
      FolioFont.googleTtf(family, style.fontWeight, style.italic)
        .orElse(fontFileForOne(family, style).map(file => NFiles.readAllBytes(file.toPath)))
    .headOption

  /** Installed TTF/OTF only (fc-match, else a font-dir scan). */
  private[util] def fontFileFor(style: PdfFolioStyle): Option[File] =
    namedFamilies(style).view.flatMap(family => fontFileForOne(family, style)).headOption

  private def namedFamilies(style: PdfFolioStyle): Seq[String] =
    cssFamilies(style.fontFamily)
      .filterNot(family => genericFamilies.contains(family.toLowerCase(Locale.ROOT)))

  private def fontFileForOne(family: String, style: PdfFolioStyle): Option[File] =
    fcMatch(family, style).orElse(scanFontDirs(family, style))

  private def loadedType0(document: PDDocument, style: PdfFolioStyle): Option[PDFont] =
    fontBytesFor(style).flatMap(bytes => loadType0(document, bytes)).filter(canEncodeDigits)

  private def loadType0(document: PDDocument, bytes: Array[Byte]): Option[PDFont] =
    if !FolioFont.isSfnt(bytes) then None
    else
      val in: ByteArrayInputStream = ByteArrayInputStream(bytes)
      try Some(PDType0Font.load(document, in, true))
      catch case NonFatal(_) => None

  private def pageFont(page: PDPage, style: PdfFolioStyle): Option[PDFont] =
    val available: Seq[PDFont] = fontsOn(page)
    cssFamilies(style.fontFamily)
      .filterNot(family => genericFamilies.contains(family.toLowerCase(Locale.ROOT)))
      .view
      .flatMap: family =>
        available.find(font => familyMatches(font, family) && weightAndStyleMatch(font, style))
      .find(canEncodeDigits)

  private def standard14(style: PdfFolioStyle): Option[PDFont] =
    cssFamilies(style.fontFamily).view.flatMap(family => standard14Name(family, style)).headOption
      .map(PDType1Font(_))

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

  private def canEncodeDigits(font: PDFont): Boolean = font match
    case _: PDType3Font => false
    case _ =>
      try
        font.getStringWidth("0123456789")
        true
      catch case NonFatal(_) => false

  private def compact(name: String): String =
    name.replaceFirst("^[A-Za-z]{6}\\+", "").toLowerCase(Locale.ROOT).replaceAll("[^a-z0-9]", "")

  private def standard14Name(family: String, style: PdfFolioStyle): Option[FontName] =
    val name: String = compact(family)
    val bold: Boolean = style.fontWeight >= 600
    val italic: Boolean = style.italic
    if name == "times" || name == "timesroman" || name == "timesnewroman" || name == "serif" then
      Some(
        if bold && italic then FontName.TIMES_BOLD_ITALIC
        else if bold then FontName.TIMES_BOLD
        else if italic then FontName.TIMES_ITALIC
        else FontName.TIMES_ROMAN
      )
    else if name == "helvetica" || name == "arial" || name == "sansserif" then
      Some(
        if bold && italic then FontName.HELVETICA_BOLD_OBLIQUE
        else if bold then FontName.HELVETICA_BOLD
        else if italic then FontName.HELVETICA_OBLIQUE
        else FontName.HELVETICA
      )
    else if name == "courier" || name == "couriernew" || name == "monospace" then
      Some(
        if bold && italic then FontName.COURIER_BOLD_OBLIQUE
        else if bold then FontName.COURIER_BOLD
        else if italic then FontName.COURIER_OBLIQUE
        else FontName.COURIER
      )
    else None

  // fontconfig CSS 400 → 80 (Regular), 700 → 200 (Bold); slant 0 roman / 100 italic.
  private def fcWeight(css: Int): Int =
    if css >= 800 then 205
    else if css >= 700 then 200
    else if css >= 600 then 180
    else if css >= 500 then 100
    else if css >= 400 then 80
    else if css >= 300 then 50
    else 40

  private def fcMatch(family: String, style: PdfFolioStyle): Option[File] =
    try
      val spec: String =
        s"$family:weight=${fcWeight(style.fontWeight)}:slant=${if style.italic then 100 else 0}"
      val process: Process = ProcessBuilder("fc-match", "-f", "%{family[0]}\u001f%{file}", spec)
        .redirectErrorStream(true)
        .start()
      val timedOut: Boolean = !process.waitFor(2, TimeUnit.SECONDS)
      if timedOut then
        process.destroyForcibly()
        None
      else if process.exitValue != 0 then None
      else
        val out: String = String(process.getInputStream.readAllBytes(), StandardCharsets.UTF_8).trim
        val parts: Array[String] = out.split('\u001f')
        if parts.length < 2 then None
        else
          val file: File = File(parts(1).trim)
          Option.when(file.isFile && compact(parts(0)) == compact(family))(file)
    catch case NonFatal(_) => None

  private def scanFontDirs(family: String, style: PdfFolioStyle): Option[File] =
    val want: String = compact(family) + compact(fileStyleSuffix(style))
    fontDirs.view.filter(_.isDirectory).flatMap(dir => findFontFile(dir, want)).headOption

  private def findFontFile(dir: File, want: String): Option[File] =
    val stream = NFiles.walk(dir.toPath, 6)
    try
      stream.iterator().asScala.map(_.toFile).find: file =>
        val name: String = file.getName
        val lower: String = name.toLowerCase(Locale.ROOT)
        (lower.endsWith(".ttf") || lower.endsWith(".otf")) &&
          compact(name.replaceFirst("\\.[^.]+$", "")) == want
    finally
      stream.close()

  private def fileStyleSuffix(style: PdfFolioStyle): String =
    val weight: String = if style.fontWeight >= 600 then "Bold" else ""
    val slant: String = if style.italic then "Italic" else ""
    val both: String = weight + slant
    if both.nonEmpty then both else "Regular"

  private def fontDirs: Seq[File] =
    val home: Option[String] = sys.props.get("user.home")
    val os: String = sys.props.getOrElse("os.name", "").toLowerCase(Locale.ROOT)
    val osDirs: Seq[File] =
      if os.contains("win") then
        Seq(File(sys.env.getOrElse("WINDIR", "C:\\Windows"), "Fonts"))
      else if os.contains("mac") then
        Seq(File("/Library/Fonts"), File("/System/Library/Fonts")) ++
          home.map(h => File(h, "Library/Fonts")).toSeq
      else
        Seq(File("/usr/share/fonts"), File("/usr/local/share/fonts"))
    osDirs ++ home.toSeq.flatMap(h => Seq(File(h, ".fonts"), File(h, ".local/share/fonts")))
