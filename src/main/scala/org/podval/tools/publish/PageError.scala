package org.podval.tools.publish

final class PageError(
  kind: PageError.Kind,
  sourcePath: Path,
  message: String,
  cause: Option[Throwable] = None
) extends Throwable(
  s"$kind: $message ($sourcePath) ${cause.map(_.getMessage).getOrElse("")}",
  cause.orNull
)

object PageError:
  trait Reporter:
    def error[R](
      kind: PageError.Kind,
      message: String,
      result: R,
      cause: Option[Throwable] = None
    ): R
  
  final class SiteReporter(
    sourcePath: Path,
    site: Site
  ) extends Reporter:
    override def error[R](
      kind: PageError.Kind,
      message: String,
      result: R,
      cause: Option[Throwable] = None
    ): R =
      site.errors.error(PageError(
        kind = kind,
        sourcePath = sourcePath,
        message = message,
        cause = cause
      ))

      result

  sealed abstract class Kind(override val toString: String)

  case object Parsing extends Kind("parsing")
  case object FileName extends Kind("file name")
  case object FileKind extends Kind("file kind")
  case object Duplicate extends Kind("duplicate")
  case object NoId extends Kind("no id")
  case object NoDate extends Kind("no date")
  case object SelfLink extends Kind("spurious external link to this site")
  case object Unresolved extends Kind("unresolved")

