package org.podval.tools.publish.js

/** A double-quoted JavaScript string literal.
  *
  * `<` becomes `\u003c` inside the literal. `org.podval.xml.XmlEncode.protectHtmlRawText`
  * later rewrites `</` across the whole script body, including resource files that never
  * went through `quote`. Those two passes stay separate.
  *
  * Escape set matches zio-blocks-html 0.0.51 `Escape.jsString`.
  */
object Js:
  def quote(s: String): String = "\"" + escape(s) + "\""

  private def escape(s: String): String =
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
