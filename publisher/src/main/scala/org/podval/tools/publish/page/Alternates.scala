package org.podval.tools.publish.page

import org.podval.metadata.Language
import org.podval.tools.publish.site.{PageError, Path, Site}

/** Other readings of one page. Two inputs: a `{base}-{xx}` sibling, and a site-root language directory.
  * The original is neither. A lone suffix or directory file is not a cluster.
  */
object Alternates:
  final class Member(
    val page: MarkupPage,
    val code: String
  )

  final class Cluster(
    val original: Member,
    val others: Seq[Member]
  ):
    def members: Seq[Member] = original +: others

  final class Index(byPage: Map[MarkupPage, Cluster]):
    def cluster(page: Page): Option[Cluster] = page match
      case markup: MarkupPage => byPage.get(markup)
      case _ => None

    def others(page: Page): Seq[Member] =
      cluster(page).toSeq.flatMap(_.members.filterNot(_.page == page))

  object Index:
    val empty: Index = new Index(Map.empty)

  /** Catalog autonym (`ru` → `русский`). An unknown code stays the code. */
  def autonym(code: String): String =
    Language.forDefaultName(code).flatMap(language => language.names.find(language.toSpec)).map(_.name)
      .getOrElse(code)

  /** Two letters after the dash, even when `forDefaultName` does not know them. */
  def suffixCode(source: Path): Option[String] =
    CollectionIndex.splitLang(source.fileName)._2.flatMap(code => MarkupPage.languageCode(Some(code)))

  /** First segment, and only when it is a catalog code (`en`, not `English`, not `zz`). */
  def directoryCode(source: Path): Option[String] =
    source.path match
      case first +: rest if rest.nonEmpty && Language.forDefaultName(first).isDefined => Some(first)
      case _ => None

  /** After the scan, once every source page and its published path exist. */
  def build(site: Site): Index =
    val authored: Seq[MarkupPage] = site.pages.pages.collect:
      case page: MarkupPage if page.source.isDefined => page
    // Agreement is reported while reading. Do it before `throwIfErrors`.
    authored.foreach(_.lang)

    val clusters: Seq[Cluster] = authored.flatMap: original =>
      val source: Path = original.sourcePath.get
      if suffixCode(source).isDefined || directoryCode(source).isDefined then None
      else
        val suffixes: Seq[MarkupPage] = authored.filter: page =>
          page.sourcePath.exists(other => isSuffixSibling(source, other))
        val directories: Seq[MarkupPage] = authored.filter: page =>
          page.sourcePath.exists(other => isDirectorySibling(source, other))
        val tagged: Seq[(MarkupPage, String)] =
          suffixes.flatMap(page => suffixCode(page.sourcePath.get).map(page -> _)) ++
            directories.flatMap(page => directoryCode(page.sourcePath.get).map(page -> _))
        val collapsed: Seq[Member] = dedupe(site, tagged.distinctBy(_._1))
        val (clashes: Seq[Member], kept: Seq[Member]) = collapsed.partition(_.code == original.lang)
        reportCodeClash(site, original, clashes)
        Option.when(kept.nonEmpty)(new Cluster(new Member(original, original.lang), kept))

    new Index(clusters.flatMap(cluster => cluster.members.map(_.page -> cluster)).toMap)

  private def isSuffixSibling(original: Path, other: Path): Boolean =
    other.extension == original.extension &&
      other.path.init == original.path.init &&
      {
        val (base: String, lang: Option[String]) = CollectionIndex.splitLang(other.fileName)
        lang.isDefined && base == original.fileName
      }

  /** `en/notes/about.md` matches `notes/about.md`. `col/en/001.xml` does not: the code is not the first segment. */
  private def isDirectorySibling(original: Path, other: Path): Boolean =
    other.extension == original.extension &&
      directoryCode(other).isDefined &&
      other.path.drop(1) == original.path

  /** Same code twice: error on each file. Keep the suffix sibling, else the first published path. */
  private def dedupe(site: Site, tagged: Seq[(MarkupPage, String)]): Seq[Member] =
    tagged.groupBy(_._2).toSeq.sortBy(_._1).map: (code, group) =>
      val pages: Seq[MarkupPage] = group.map(_._1)
      if pages.length > 1 then
        val listed: String = pages.flatMap(_.sourcePath).map(_.toString).mkString(", ")
        pages.foreach: page =>
          page.sourcePath.foreach: source =>
            site.error(source, PageError.FileName, s"Duplicate language $code: $listed")
      val kept: MarkupPage =
        if pages.length == 1 then pages.head
        else pages.find(page => page.sourcePath.exists(source => suffixCode(source).isDefined))
          .getOrElse(pages.minBy(_.publishedPath))
      new Member(kept, code)

  private def reportCodeClash(site: Site, original: MarkupPage, clashes: Seq[Member]): Unit =
    clashes.foreach: member =>
      val listed: String = Seq(original.sourcePath, member.page.sourcePath).flatten.map(_.toString).mkString(", ")
      original.sourcePath.foreach: source =>
        site.error(source, PageError.FileName, s"Duplicate language ${member.code}: $listed")
      member.page.sourcePath.foreach: source =>
        site.error(source, PageError.FileName, s"Duplicate language ${member.code}: $listed")
