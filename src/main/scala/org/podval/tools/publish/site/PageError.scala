package org.podval.tools.publish.site

final class PageError(
  sourcePath: Path,
  val kind: PageError.Kind,
  message: String,
  cause: Option[Throwable]
) extends Throwable(
  s"$kind: $message ($sourcePath) ${cause.map(_.getMessage).getOrElse("")}",
  cause.orNull
)

object PageError:
  sealed abstract class Kind(override val toString: String)

  case object MalformedFrontMatter extends Kind("malformed frontmatter")
  case object AmbiguousFrontMatter extends Kind("ambiguous frontmatter")
  case object AmbiguousTitle extends Kind("ambiguous title")
  case object MalformedXml extends Kind("malformed XML")
  case object FileName extends Kind("file name")
  case object Duplicate extends Kind("duplicate")
  case object NoId extends Kind("no id")
  case object NoTitle extends Kind("no title")
  case object NoDate extends Kind("no date")
  case object SelfLink extends Kind("spurious external link to this site")
  case object Unresolved extends Kind("unresolved")
  case object MissingAsset extends Kind("missing asset")
  case object UnknownCitation extends Kind("unknown citation")

  val all: List[Kind] = List(
    MalformedFrontMatter,
    AmbiguousFrontMatter,
    AmbiguousTitle,
    MalformedXml,
    FileName,
    Duplicate,
    NoId,
    NoTitle,
    NoDate,
    SelfLink,
    Unresolved,
    MissingAsset,
    UnknownCitation
  )
