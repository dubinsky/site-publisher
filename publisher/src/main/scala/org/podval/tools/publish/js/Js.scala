package org.podval.tools.publish.js

/** Raw JavaScript. `js"..."` quotes `String` holes and splices `Js` holes. */
// TODO move to XML library (or eliminate)
final class Js private (val value: String):
  override def toString: String = value
  def stripMargin: Js = Js(value.stripMargin)

object Js:
  def apply(value: String): Js = new Js(value)

  private[js] def interpolate(parts: Seq[String], args: Seq[Matchable]): String =
    val spliced: Seq[String] = args.map:
      case s: String => "\"" + escapeJsString(s) + "\""
      case j: Js => j.value
      case other =>
        throw IllegalArgumentException(
          s"js interpolator hole must be String or Js, got ${other.getClass.getName}"
        )
    parts.iterator.zipAll(spliced.iterator, "", "").map((part, arg) => part + arg).mkString

  // Same set as zio-blocks-html 0.0.51 Escape.jsString / jsStringTo.
  private def escapeJsString(s: String): String =
    val (prefix, rest) = s.span(c => !needsEscape(c))
    if rest.isEmpty then s
    else
      val sb: StringBuilder = StringBuilder(s.length + 16).append(prefix)
      rest.foreach: c =>
        if needsEscape(c) then sb.append(escapeChar(c))
        else sb.append(c)
      sb.toString

  private def needsEscape(c: Char): Boolean =
    c == '"' || c == '\'' || c == '\\' || c == '\n' || c == '\r' || c == '\t' ||
      c == '<' || c == '>' || c == '&' || c == '\u2028' || c == '\u2029' || c < 32

  private def escapeChar(c: Char): String =
    c match
      case '"' => "\\\""
      case '\'' => "\\'"
      case '\\' => "\\\\"
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case '<' => "\\u003c"
      case '>' => "\\u003e"
      case '&' => "\\u0026"
      case '\u2028' => "\\u2028"
      case '\u2029' => "\\u2029"
      case _ => "\\u%04x".format(c.toInt)

extension (sc: StringContext)
  def js(args: Matchable*): Js = Js(Js.interpolate(sc.parts, args))
