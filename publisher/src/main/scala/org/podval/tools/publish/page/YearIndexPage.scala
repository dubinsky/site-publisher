package org.podval.tools.publish.page

import org.podval.metadata.Language
import org.podval.tools.publish.markup.{PlainText, TeiDate}
import org.podval.tools.publish.site.Site
import org.podval.tools.publish.util.Icon
import org.podval.xml.Xml
import org.podval.xml.dsl.{*, given}

/** Synthetic `{root}-years.html`: original TEI documents under one root store, by creation-date year. */
final class YearIndexPage(
  site: Site,
  root: Page,
  documents: Seq[Page]
) extends SyntheticMarkupPage(site, StoreIndexes.pagePath(root, StoreIndexes.yearsSuffix)):
  override def titleDefault: String =
    if russian then "Годы" else "Years"

  override protected def iconDefault: Icon = Icon.calendar

  override def parent: Option[DirectoryPage] = None

  override def pageHeader: Option[Xml.Element] = Some(
    header(className := "page-header",
      h1(className := "page-title", title)
    )
  )

  override def prev: Option[Page] = None
  override def next: Option[Page] = None

  override protected def syntheticContent: Xml.Element =
    val rows: Seq[(Page, TeiDate.Filing)] = documents.map(page => page -> filingOf(page))
    div(className := "year-index", yearSections(rows) ++ undatedSection(rows))

  private def russian: Boolean =
    site.languageSpec == Language.Russian.toSpec

  private def undatedHeading: String =
    if russian then "Без даты" else "Undated"

  private def filingOf(page: Page): TeiDate.Filing =
    page.doc.flatMap(_.documentHeader).flatMap(_.date).fold(
      TeiDate.Filing(Seq.empty, "", _ => (0, 0))
    )(TeiDate.filingYears)

  private def yearSections(rows: Seq[(Page, TeiDate.Filing)]): Seq[Xml.Element] =
    val dated: Seq[(Int, Page, TeiDate.Filing)] =
      rows.flatMap: (page, filing) =>
        filing.years.map(year => (year, page, filing))
    val byYear: Map[Int, Seq[(Int, Page, TeiDate.Filing)]] = dated.groupBy(_._1)
    byYear.keys.toSeq.sorted.map: year =>
      val inYear: Seq[(Page, TeiDate.Filing)] =
        byYear(year).map((_, page, filing) => (page, filing))
          .sortBy((page, filing) => sortKey(year, page, filing))
      section(s"y$year", year.toString, inYear)

  private def undatedSection(rows: Seq[(Page, TeiDate.Filing)]): Seq[Xml.Element] =
    val undated: Seq[(Page, TeiDate.Filing)] =
      rows.filter((_, filing) => filing.years.isEmpty).sortBy((page, _) => undatedKey(page))
    Option.when(undated.nonEmpty)(section("undated", undatedHeading, undated)).toSeq

  private def sortKey(year: Int, page: Page, filing: TeiDate.Filing): (Int, Int, String, String, String) =
    val (month, day) = filing.sortIn(year)
    (month, day, archiveId(page), pathLabel(page), sourceOf(page))

  private def undatedKey(page: Page): (String, String, String) =
    (archiveId(page), pathLabel(page), sourceOf(page))

  private def archiveId(page: Page): String =
    val chain: Seq[Page] =
      (PageHeader.collectorAncestors(page) :+ page).filter(_.store.isDefined)
    chain.dropWhile(_.path == root.path).headOption.flatMap(_.sourcePath.map(_.fileName)).getOrElse("")

  private def pathLabel(page: Page): String =
    StoreIndexes.pathHeaderHorizontal(page, root)

  private def sourceOf(page: Page): String =
    page.sourcePath.map(_.toString).getOrElse("")

  private def linkText(page: Page): String =
    val header: String = pathLabel(page)
    val base: String = CollectionIndex.baseName(page)
    if header.isEmpty then base else s"$header, $base"

  private def descriptionText(page: Page): Option[String] =
    page.doc.flatMap(_.documentHeader).flatMap(_.description)
      .map(xml => PlainText.visible(xml, skipNotes = true, skipTips = false))
      .filter(_.nonEmpty)

  private def section(sectionId: String, heading: String, rows: Seq[(Page, TeiDate.Filing)]): Xml.Element =
    element(
      "section",
      className := "year-group",
      id := sectionId,
      h2(heading),
      ul(rows.map((page, filing) => row(page, filing)))
    )

  private def row(page: Page, filing: TeiDate.Filing): Xml.Element =
    li(
      className := "year-row",
      span(className := "date", filing.label),
      NamedWindows.setXmlTarget(
        a(className := "archive-path", href := page.publishedPath.toString, linkText(page)),
        page
      ),
      descriptionText(page).map(text => p(className := "description", text))
    )
