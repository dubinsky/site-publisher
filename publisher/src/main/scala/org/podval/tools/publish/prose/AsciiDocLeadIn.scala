package org.podval.tools.publish.prose

/** AsciiDoc block openers that must not land at the beginning of a line. */
object AsciiDocLeadIn:
  private val prefix: List[scala.util.matching.Regex] = List(
    """^(?://|====|----|\.\.\.\.|____|\*\*\*\*|\+\+\+\+|////|--|\|===|```)""".r,
    """^(?:'''|<<<|---|\*\*\*|___)""".r,
    """^\[.*\]$""".r,
    """^:[^:\s]+:""".r,
    """^(?:include|ifdef|ifndef|ifeval|endif)::""".r,
    """^//""".r,
    """^(?:NOTE|TIP|IMPORTANT|CAUTION|WARNING):[ \t]""".r,
    """^\.(?![ \t])""".r,
    """^(?:\*+|-+|\.{1,5}|\d{1,9}\.)[ \t]""".r,
    """^[^\s].*::[ \t]""".r,
    """^<\d+>[ \t]""".r,
    """^={1,6}[ \t]""".r,
    """^#{1,6}[ \t]""".r
  )

  def matches(remainder: String): Boolean =
    prefix.exists(_.findPrefixMatchOf(remainder).isDefined)
