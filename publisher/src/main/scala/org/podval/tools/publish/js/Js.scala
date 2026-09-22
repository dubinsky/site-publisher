package org.podval.tools.publish.js

/** Raw JavaScript. `js"..."` quotes `String` holes and splices `Js` holes.
  *
  * Stays in site-publisher. The interpolator builds a JavaScript source string.
  * It never builds an element, and the writer never sees `Js`. `JSLibrary` unwraps
  * `.value` into `script().inlineJs`, which appends a text child. After that call a
  * quoted hole and a classpath resource are the same text node. Moving `Js` into
  * `org.podval.xml` without changing that signature only changes the import.
  *
  * The production holes are the analytics id, the Mermaid URL, and the Cytoscape URL
  * plus `/assets/js/graph.js`. MathJax and Highlights use `js"..."` with no holes.
  * `Site` wraps `siteSettings.js` in `Js` because `headInlineJs` is `Option[Js]`.
  * Nothing outside `JsSpec` splices a `Js` hole. The only caller is site-publisher.
  *
  * A `String` hole escapes `<` to `\u003c` inside a JavaScript literal.
  * `protectHtmlRawText` later rewrites `</` across the whole script body, including
  * resource files that never went through `js"..."`. Those two passes stay separate.
  *
  * `inlineJs` and `externalJs` on the XML DSL set a text child and a `src` attribute.
  * This interpolator is the code that quotes JavaScript literals.
  *
  * A shorter publisher stays local too: `Option[String]` on `JSLibrary`, a quoter for
  * those three holes, and `siteSettings.js` as the string `Files.readResource` already
  * returns.
  */
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
