package org.podval.tools.publish.page

final class PdfPageSize(
  val widthIn: Double,
  val heightIn: Double,
  val marginTopIn: Double,
  val marginSideIn: Double,
  val marginBottomIn: Double
):
  // Content box inside margins — viewport and TOC page math must match this, not full paper.
  val contentWidthPx: Int =
    Math.round((widthIn - 2 * marginSideIn) * PdfPageSize.cssPxPerIn).toInt

  val contentHeightPx: Int =
    Math.round((heightIn - marginTopIn - marginBottomIn) * PdfPageSize.cssPxPerIn).toInt

  val sideInset: Float =
    (marginSideIn * PdfPageSize.pointsPerInch).toFloat

  val baselineFromBottom: Float =
    (marginBottomIn / 2 * PdfPageSize.pointsPerInch).toFloat

object PdfPageSize:
  val cssPxPerIn: Double = 96.0 // Playwright/Chromium default
  val pointsPerInch: Float = 72f

  // Bottom/side inset is also the folio stamp area (see PdfPageNumbers).
  val letter: PdfPageSize = PdfPageSize(
    widthIn = 8.5,
    heightIn = 11.0,
    marginTopIn = 0.5,
    marginSideIn = 0.5,
    marginBottomIn = 0.6
  )
