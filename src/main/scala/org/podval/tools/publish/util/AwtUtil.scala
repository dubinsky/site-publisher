package org.podval.tools.publish.util

import java.awt.Color

object AwtUtil:
  def parseColor(css: String): Color =
    val s: String = css.trim
    if s.startsWith("#") then
      val hex: String = s.substring(1)
      val full: String =
        if hex.length == 3 then hex.flatMap(c => s"$c$c")
        else if hex.length >= 6 then hex.substring(0, 6)
        else hex
      Color.decode("#" + full)
    else
      val rgb = """rgba?\(\s*([\d.]+)\s*,\s*([\d.]+)\s*,\s*([\d.]+)""".r
      s match
        case rgb(r, g, b) =>
          Color(
            clampByte(r.toFloat),
            clampByte(g.toFloat),
            clampByte(b.toFloat)
          )
        case _ => throw IllegalArgumentException(s"Unrecognized CSS color: $css")

  private def clampByte(value: Float): Int =
    math.max(0, math.min(255, math.round(value)))
