package org.podval.tools.publish.util

object Json:
  def string(s: String): String =
    val escaped: String = s.flatMap:
      case '"' => "\\\""
      case '\\' => "\\\\"
      case '\b' => "\\b"
      case '\f' => "\\f"
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case '<' => "\\u003c"
      case '>' => "\\u003e"
      case '&' => "\\u0026"
      case c if c < 32 => f"\\u${c.toInt}%04x"
      case c => c.toString
    s"\"$escaped\""
