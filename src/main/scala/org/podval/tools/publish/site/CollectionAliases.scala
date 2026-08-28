package org.podval.tools.publish.site

import org.podval.tools.publish.markup.{CollectionIndex, Facsimile, HtmlMarkup}
import org.podval.tools.publish.util.Files
import java.io.File

/** Alias prefix table for Cloudflare Worker / inspection. Same map as `Pages.rewriteRequest`. */
object CollectionAliases:
  val fileName: String = "collection-aliases.json"

  final class Entry(
    val from: Seq[String],
    val to: Seq[String],
    val index: Path
  )

  def entries(pages: Pages): Seq[Entry] =
    pages.collectionAliasEntries.sortBy(e => (-e.from.length, e.from.mkString("/")))

  def rewrite(request: Path, table: Seq[Entry]): Option[Path] =
    val segments: Seq[String] = request.path
    table
      .filter(e => segments.startsWith(e.from))
      .maxByOption(_.from.length)
      .map: e =>
        val remainder: Seq[String] = segments.drop(e.from.length)
        val segs: Seq[String] = originalFacsimile(Facsimile.inboundRemainder(remainder).getOrElse(remainder))
        if segs.isEmpty then e.index
        else Path(e.to ++ segs, request.extension.orElse(Some(HtmlMarkup.extension)))

  def json(table: Seq[Entry]): String =
    val body: String = table.map: e =>
      val indexExt: String = e.index.extension.map(ext => s""""$ext"""").getOrElse("null")
      s"""{"from":${strings(e.from)},"to":${strings(e.to)},"index":{"path":${strings(e.index.path)},"extension":$indexExt}}"""
    .mkString(",")
    s"""{"aliases":[$body]}\n"""

  def write(targetDirectory: File, pages: Pages): Unit =
    Files.write(File(targetDirectory, fileName), json(entries(pages)))

  private def originalFacsimile(remainder: Seq[String]): Seq[String] =
    remainder match
      case Seq(name, Facsimile.fileName) =>
        Seq(CollectionIndex.splitLang(name)._1, Facsimile.fileName)
      case other => other

  private def strings(values: Seq[String]): String =
    values.map(s => s"\"${escape(s)}\"").mkString("[", ",", "]")

  private def escape(value: String): String =
    value.flatMap:
      case '\\' => "\\\\"
      case '"' => "\\\""
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case c => c.toString
