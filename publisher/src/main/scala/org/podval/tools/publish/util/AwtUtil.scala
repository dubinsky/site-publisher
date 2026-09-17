package org.podval.tools.publish.util

import java.awt.Color

object AwtUtil:
  def parseColor(css: String): Color =
    val s: String = css.trim
    hexColor(s)
      .orElse(rgbColor(s))
      .orElse(oklabColor(s))
      .getOrElse(throw IllegalArgumentException(s"Unrecognized CSS color: $css"))

  private def hexColor(s: String): Option[Color] =
    if !s.startsWith("#") then None else
      val hex: String = s.substring(1)
      val full: String =
        if hex.length == 3 then hex.flatMap(c => s"$c$c")
        else if hex.length >= 6 then hex.substring(0, 6)
        else hex
      Some(Color.decode("#" + full))

  private val rgb = """rgba?\(([^)/]+)\)""".r

  private def rgbColor(s: String): Option[Color] = s match
    case rgb(inside) =>
      val parts: Array[String] = inside.split("[,\\s]+").filter(_.nonEmpty).map(_.stripSuffix("%"))
      if parts.length >= 3 then
        Some(Color(clampByte(parts(0).toFloat), clampByte(parts(1).toFloat), clampByte(parts(2).toFloat)))
      else None
    case _ => None

  // oklab(L a b) or oklab(L a b / alpha); L,a,b are unitless.
  private val oklab =
    """oklab\(\s*([+\-\d.]+)\s+([+\-\d.]+)\s+([+\-\d.]+)(?:\s*/\s*[+\-\d.%]+)?\s*\)""".r

  private def oklabColor(s: String): Option[Color] = s match
    case oklab(l, a, b) => Some(oklabToSrgb(l.toDouble, a.toDouble, b.toDouble))
    case _ => None

  // Björn Ottosson, https://bottosson.github.io/posts/oklab/
  private def oklabToSrgb(L: Double, a: Double, b: Double): Color =
    val l_ : Double = L + 0.3963377774 * a + 0.2158037573 * b
    val m_ : Double = L - 0.1055613458 * a - 0.0638541728 * b
    val s_ : Double = L - 0.0894841775 * a - 1.2914855480 * b
    val l: Double = l_ * l_ * l_
    val m: Double = m_ * m_ * m_
    val s: Double = s_ * s_ * s_
    val rLin: Double = +4.0767416621 * l - 3.3077115913 * m + 0.2309699292 * s
    val gLin: Double = -1.2684380046 * l + 2.6097574011 * m - 0.3413193965 * s
    val bLin: Double = -0.0041960863 * l - 0.7034186147 * m + 1.7076147010 * s
    Color(
      clampByte((linearToSrgb(rLin) * 255).toFloat),
      clampByte((linearToSrgb(gLin) * 255).toFloat),
      clampByte((linearToSrgb(bLin) * 255).toFloat)
    )

  private def linearToSrgb(c: Double): Double =
    val x: Double = math.max(0.0, math.min(1.0, c))
    if x <= 0.0031308 then 12.92 * x
    else 1.055 * math.pow(x, 1.0 / 2.4) - 0.055

  private def clampByte(value: Float): Int =
    math.max(0, math.min(255, math.round(value)))
