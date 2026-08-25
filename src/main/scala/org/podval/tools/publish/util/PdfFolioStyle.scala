package org.podval.tools.publish.util

import com.microsoft.playwright.Page
import scala.jdk.CollectionConverters.MapHasAsScala
import java.awt.Color

/** Computed `.folio` used by PdfPageNumbers (`font-family` stack, size, weight,
  * italic, color). Defaults to body type in layout.css; override `.folio` in CSS.
  * Outer-edge position is not taken from CSS. */
final class PdfFolioStyle(
  val fontFamily: String,
  val fontSizePt: Float,
  val fontWeight: Int,
  val italic: Boolean,
  val color: Color
)

object PdfFolioStyle:
  lazy val js: String = Files.readResource("/org/podval/tools/publish/util/pdfFolioStyle.js")

  val fallback: PdfFolioStyle = PdfFolioStyle(
    fontFamily = "Times",
    fontSizePt = 10f,
    fontWeight = 400,
    italic = false,
    color = Color(0x33, 0x33, 0x33)
  )
  
  def forPage(page: Page): PdfFolioStyle =
    val raw = page.evaluate(s"(() => { ${PdfFolioStyle.js}; return pdfFolioStyle(); })()")
      .asInstanceOf[java.util.Map[String, Matchable]]
      .asScala
      .toMap

    def str(key: String): String = raw.get(key).map(String.valueOf).getOrElse("")

    def num(key: String): Double = raw.get(key) match
      case Some(n: java.lang.Number) => n.doubleValue
      case Some(s: String) => s.toDouble
      case _ => 14.0

    def bool(key: String): Boolean = raw.get(key) match
      case Some(b: java.lang.Boolean) => b.booleanValue
      case Some(s: String) => s.toBoolean
      case _ => false

    PdfFolioStyle(
      fontFamily = str("fontFamily"),
      fontSizePt = (num("fontSizePx") * PdfPageSize.pointsPerInch / PdfPageSize.cssPxPerIn).toFloat,
      fontWeight = num("fontWeight").toInt,
      italic = bool("italic"),
      color = AwtUtil.parseColor(str("color"))
    )
