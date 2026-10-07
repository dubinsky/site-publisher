package org.podval.tools.publish.prose

/** CommonMark block starts that must not land at the beginning of a line. */
object MarkdownLeadIn:
  private val type6Tags: Set[String] = Set(
    "blockquote", "figcaption", "basefont", "colgroup", "fieldset", "frameset", "menuitem", "noframes",
    "optgroup", "address", "article", "caption", "details", "section", "summary", "center", "dialog",
    "figure", "footer", "header", "iframe", "legend", "option", "source", "aside", "frame", "param",
    "table", "tbody", "tfoot", "thead", "title", "track", "base", "body", "form", "head", "html",
    "link", "main", "menu", "meta", "col", "dir", "div", "nav", "dd", "dl", "dt", "h1", "h2", "h3",
    "h4", "h5", "h6", "hr", "li", "ol", "td", "th", "tr", "ul", "p"
  )

  private val type6Tag = """^ {0,3}</?([A-Za-z0-9]+)""".r

  private val prefix: List[scala.util.matching.Regex] = List(
    """^ {0,3}#{1,6}([ \t]|$)""".r,
    """^ {0,3}[-*+]([ \t]|$)""".r,
    """^ {0,3}[0-9]{1,9}[.)]([ \t]|$)""".r,
    """^ {0,3}>""".r,
    """^ {0,3}(`{3,}|~{3,})""".r,
    """(?i)^ {0,3}</?(?:script|pre|style)(?:[ \t>]|$)""".r,
    """^ {0,3}<!--""".r,
    """^ {0,3}<\?""".r,
    """^ {0,3}<![A-Z]""".r,
    """^ {0,3}<!\[CDATA\[""".r,
    """^ {0,3}\[[^\]\n]+\]:""".r,
    """^ {0,3}\|""".r
  )

  private val wholeLine: List[String] = List(
    """^ {0,3}(?:-[ \t]*){3,}$""",
    """^ {0,3}(?:_[ \t]*){3,}$""",
    """^ {0,3}(?:\*[ \t]*){3,}$""",
    """^ {0,3}(?:=+|-+)[ \t]*$"""
  )

  def matches(remainder: String): Boolean =
    prefix.exists(_.findPrefixMatchOf(remainder).isDefined) ||
      wholeLine.exists(remainder.matches) ||
      type6(remainder)

  private def type6(remainder: String): Boolean =
    type6Tag.findPrefixMatchOf(remainder).exists: matched =>
      val name: String = matched.group(1).toLowerCase
      val after: String = remainder.substring(matched.end)
      type6Tags.contains(name) && (after.isEmpty || after.startsWith(">") || after.startsWith("/>") ||
        after.charAt(0) == ' ' || after.charAt(0) == '\t')
