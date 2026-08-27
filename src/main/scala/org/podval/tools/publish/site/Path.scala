package org.podval.tools.publish.site

import org.podval.tools.publish.markup.HtmlMarkup
import org.podval.tools.publish.util.Files
import java.io.File

final case class Path(
  path: Seq[String],
  extension: Option[String] = None
) derives CanEqual:
  def fileName: String = path.last

  override def equals(obj: Any): Boolean = obj.asInstanceOf[Matchable] match
    case that: Path => this.path == that.path && this.extension == that.extension
    case _ => false
    
  override def toString: String = path.mkString("/", "/", extensionString)

  def add(segment: String): Path = copy(path = path :+ segment)
  
  def extensionString: String = extension match
    case None => ""
    case Some(extension) => s".$extension"

  def withExtension(extension: String): Path =
    if this.extension.contains(extension)
    then this
    else this.copy(extension = Some(extension))

  def withoutHtml: Path =
    if this.extension.contains(HtmlMarkup.extension)
    then this.copy(extension = None)
    else this
    
  def html: Path = withExtension(HtmlMarkup.extension)

  def file(directory: File): File = file(directory, path)
  
  @scala.annotation.tailrec
  private def file(directory: File, path: Seq[String]): File =
    if path.isEmpty then directory
    else if path.length == 1 then File(directory, path.head + extensionString)
    else file(File(directory, path.head), path.tail)

  def relativize(alias: String): Path =
    val extra: Seq[String] = alias.split("/").toSeq.filterNot(_.isEmpty)
    val base: Seq[String] = if alias.startsWith("/") then Seq.empty else path.init
    Path(Path.normalize(base ++ extra) *)

  // Join a file href (`index.html`, `./x.html`, `/a/b.html`) to this page's directory.
  // Wiki names (`notes`, `book/book`) are not resolved here.
  def resolveFrom(href: String): Path =
    val extra: Path = Path.fromHref(href)
    if href.trim.startsWith("/") then extra.copy(path = Path.normalize(extra.path))
    else
      val base: Seq[String] = if path.isEmpty then Seq.empty else path.init
      Path(Path.normalize(base ++ extra.path), extra.extension)

object Path:
  given Ordering[Path] = (left: Path, right: Path) =>
    Ordering.Implicits.seqOrdering[Seq, String].compare(left.path, right.path)

  val root: Path = new Path(Seq.empty, None)

  def apply(path: String*) = new Path(path, None)

  def fromHref(href: String): Path =
    val segments: Seq[String] = href.split("/").toSeq.map(_.trim).filterNot(_.isEmpty)
    if segments.isEmpty then Path.root
    else
      val last: String = segments.last
      val (name: String, extension: Option[String]) = Files.nameAndExtension(last)
      // Digit-only suffix is part of the name (`255.2`), not an extension.
      if extension.exists(ext => ext.nonEmpty && ext.forall(_.isDigit))
      then Path(segments.init :+ last.trim, None)
      else Path(segments.init :+ name.trim, extension)

  // Relative `*.html` and `./` / `../` hrefs; not wiki names and not site-root `/…`.
  def isRelativeFileHref(href: String): Boolean =
    val trimmed: String = href.trim
    !trimmed.startsWith("/") && (
      trimmed.startsWith(".") ||
        Files.nameAndExtension(
          trimmed.split('/').map(_.trim).filterNot(_.isEmpty).lastOption.getOrElse(trimmed)
        )._2.contains(HtmlMarkup.extension)
    )

  // Drop `.`; `..` pops a segment. Extra `..` at site root is ignored (does not escape).
  private[site] def normalize(segments: Seq[String]): Seq[String] =
    segments.foldLeft(List.empty[String]): (acc, seg) =>
      seg match
        case "." => acc
        case ".." => if acc.isEmpty then acc else acc.init
        case s => acc :+ s
