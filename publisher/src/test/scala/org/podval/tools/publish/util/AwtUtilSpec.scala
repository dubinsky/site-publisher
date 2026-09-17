package org.podval.tools.publish.util

import org.scalatest.funsuite.AnyFunSuite
import java.awt.Color

final class AwtUtilSpec extends AnyFunSuite:
  given CanEqual[Color, Color] = CanEqual.derived

  test("parseColor understands hex and rgb") {
    assert(AwtUtil.parseColor("#333333") == Color(0x33, 0x33, 0x33))
    assert(AwtUtil.parseColor("rgb(51, 51, 51)") == Color(51, 51, 51))
    assert(AwtUtil.parseColor("rgb(51 51 51)") == Color(51, 51, 51))
  }

  test("parseColor understands oklab from Chromium computed style") {
    val color: Color = AwtUtil.parseColor("oklab(0.452433 0.0000206307 0.00000905246)")
    assert(math.abs(color.getRed - color.getGreen) <= 1)
    assert(math.abs(color.getGreen - color.getBlue) <= 1)
    assert(color.getRed > 40 && color.getRed < 140)
  }
