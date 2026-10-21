package org.podval.tools.publish.site

import org.podval.metadata.{Language, Names}

/** Interface strings. English is first in each message, so [Names.doFind] falls back to it.
  * Selectors are not messages: `text` fills a term that is not an axis. */
object Messages:
  def text(key: String, values: (String, Any)*)(using spec: Language.Spec): String =
    fill(messages(key).doFind(spec).name, values.iterator.map((k, v) => k -> v.toString).toMap)

  private[site] lazy val messages: Map[String, Names] =
    Terms.catalog.terms.filterNot(_.selector).map(term => term.id -> term.names).toMap

  /** A `{name}` with no supplied value throws. Extra values are ignored. Values are not re-scanned. */
  private def fill(template: String, values: Map[String, String]): String =
    def loop(rest: String): String = rest.span(_ != '{') match
      case (head, "") => head
      case (head, tail) =>
        val close: Int = tail.indexOf('}')
        if close < 0 then throw IllegalArgumentException(s"Unclosed brace in message: $template")
        val name: String = tail.substring(1, close)
        val value: String = values.getOrElse(name, throw IllegalArgumentException(
          s"No value for {$name} in message: $template"
        ))
        head + value + loop(tail.substring(close + 1))
    loop(template)
