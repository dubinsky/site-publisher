package org.podval.tools.publish.page

import org.opentorah.calendar.roman.Julian
import org.podval.metadata.Language
import org.podval.tools.publish.markup.{PlainText, TeiDate}
import org.podval.tools.publish.site.{Messages, Path, Site}
import org.podval.tools.publish.util.Icon
import org.podval.xml.Xml
import org.podval.xml.dsl.{*, given}
import scala.math.Ordering.Implicits.infixOrderingOps

/** Synthetic `{root}-years.html`: original TEI documents under one root store.
  * A closed year line, then a closed month line. A day line appears when three or more
  * documents share that exact day. Undated documents are grouped by the archive under the root.
  * A file at this path is rendered above the list. */
final class YearIndexPage(
  site: Site,
  root: Page,
  documents: Seq[Page]
) extends SyntheticMarkupPage(site, StoreIndexes.pagePath(root, StoreIndexes.yearsSuffix)):
  override def leafIndex: Boolean = true

  private val DayHeadingMinimum: Int = 3

  // Calendar month names are nominative. A Russian day line needs the genitive.
  private val russianGenitive: Map[Int, String] = Map(
    1 -> "января",
    2 -> "февраля",
    3 -> "марта",
    4 -> "апреля",
    5 -> "мая",
    6 -> "июня",
    7 -> "июля",
    8 -> "августа",
    9 -> "сентября",
    10 -> "октября",
    11 -> "ноября",
    12 -> "декабря"
  )

  override def titleDefault: String =
    given Language.Spec = site.languageSpec
    site.terms.pluralHeading("year")

  override protected def iconDefault: Icon = Icon.calendar

  override def parent: Option[DirectoryPage] = None

  override def pageHeader: Option[Xml.Element] = Some(
    header(className := "page-header",
      h1(className := "page-title", title)
    )
  )

  override def prev: Option[Page] = None
  override def next: Option[Page] = None

  override def markupContent: Option[Xml.Element] =
    markupContent(sectionId = None, isTerminal = true)

  override protected def syntheticContent: Xml.Element =
    val rows: Seq[(Page, TeiDate.Filing)] = documents.map(page => page -> filingOf(page))
    div(className := "year-index").setChildren(yearSections(rows) ++ undatedSection(rows))

  private def undatedHeading: String =
    given Language.Spec = site.languageSpec
    Messages.text("years.undated")

  private def noMonth: String =
    given Language.Spec = site.languageSpec
    Messages.text("years.noMonth")

  private def filingOf(page: Page): TeiDate.Filing =
    page.doc.flatMap(_.documentHeader).flatMap(_.date).fold(TeiDate.emptyFiling)(TeiDate.filingYears)

  private def yearSections(rows: Seq[(Page, TeiDate.Filing)]): Seq[Xml.Element] =
    val dated: Seq[(Int, Page, TeiDate.Filing)] =
      rows.flatMap: (page, filing) =>
        filing.years.map(year => (year, page, filing))
    val byYear: Map[Int, Seq[(Int, Page, TeiDate.Filing)]] = dated.groupBy(_._1)
    byYear.keys.toSeq.sorted.map: year =>
      val inYear: Seq[(Page, TeiDate.Filing)] =
        byYear(year).map((_, page, filing) => (page, filing))
      yearDisclosure(year, inYear)

  private def yearDisclosure(year: Int, inYear: Seq[(Page, TeiDate.Filing)]): Xml.Element =
    val placed: Seq[Row] =
      inYear.flatMap: (page, filing) =>
        filing.slotsIn(year).map(slot => Row(page, filing, slot))
    val byMonth: Map[Int, Seq[Row]] = placed.groupBy(_.slot.month)
    val months: Seq[Xml.Element] =
      byMonth.keys.toSeq.sortBy(monthOrder).map(month => monthDisclosure(year, month, byMonth(month)))
    disclosure("year-group", s"y$year", counted(year.toString, inYear.length), months)

  private def monthOrder(month: Int): Int =
    if month == 0 then Int.MaxValue else month

  private def monthDisclosure(year: Int, month: Int, rows: Seq[Row]): Xml.Element =
    val name: String = if month == 0 then noMonth else monthName(month)
    disclosure("month-group", s"y$year-m$month", counted(name, rows.length), monthBody(month, rows))

  private def monthBody(month: Int, rows: Seq[Row]): Seq[Xml.Element] =
    val sorted: Seq[Row] = rows.sortBy(sortKey)
    val headedDays: Set[Int] =
      sorted.filter(_.slot.groupsDay).groupBy(_.slot.day).collect:
        case (day, group) if group.length >= DayHeadingMinimum => day
      .toSet
    val byDay: Map[Int, Seq[Row]] = sorted.groupBy(_.slot.day)
    val blocks: Seq[Block] = byDay.keys.toSeq.sorted.flatMap: day =>
      val inDay: Seq[Row] = byDay(day).sortBy(sortKey)
      if !headedDays.contains(day) then Seq(Block(None, inDay))
      else
        val (headed, plain) = inDay.partition(_.slot.groupsDay)
        val (before, after) = plain.span(row => sortKey(row) < sortKey(headed.head))
        Seq(Block(None, before), Block(Some(day), headed), Block(None, after)).filter(_.rows.nonEmpty)
    mergePlain(blocks).flatMap(renderBlock(month, _))

  private def renderBlock(month: Int, block: Block): Seq[Xml.Element] =
    val list: Xml.Element = ul(block.rows.map(row => this.row(row.page, row.filing)))
    block.day match
      case Some(day) =>
        Seq(p(className := "day-heading", dayHeading(month, day, block.rows.length)), list)
      case None =>
        Seq(list)

  private def mergePlain(blocks: Seq[Block]): Seq[Block] =
    blocks.foldLeft(List.empty[Block]): (acc, block) =>
      acc match
        case head :: tail if head.day.isEmpty && block.day.isEmpty =>
          head.copy(rows = head.rows ++ block.rows) :: tail
        case _ =>
          block :: acc
    .reverse

  private def undatedSection(rows: Seq[(Page, TeiDate.Filing)]): Seq[Xml.Element] =
    val undated: Seq[(Page, TeiDate.Filing)] =
      rows.filter((_, filing) => filing.years.isEmpty).sortBy((page, _) => undatedKey(page))
    if undated.isEmpty then Seq.empty
    else
      val order: Map[Path, Int] =
        StoreTree.childPages(root).zipWithIndex.map((page, index) => page.path -> index).toMap
      val groups: Map[Option[Page], Seq[(Page, TeiDate.Filing)]] =
        undated.groupBy((page, _) => archiveOf(page))
      val archives: Seq[Xml.Element] =
        groups.toSeq.sortBy: (archive, _) =>
          (archive.flatMap(page => order.get(page.path)).getOrElse(Int.MaxValue), archiveLabel(archive))
        .map: (archive, members) =>
          val idSuffix: String = archive.flatMap(_.sourcePath.map(_.fileName)).getOrElse("unknown")
          disclosure(
            "archive-group",
            s"undated-$idSuffix",
            counted(archiveLabel(archive), members.length),
            Seq(ul(members.map((page, filing) => row(page, filing))))
          )
      Seq(disclosure("year-group", "undated", counted(undatedHeading, undated.length), archives))

  private def disclosure(cls: String, elementId: String, label: String, body: Seq[Xml.Element]): Xml.Element =
    val node: Xml.Element = details(className := cls, id := elementId, summary(label))
    node.setChildren(node.getChildren ++ body)

  private def counted(label: String, count: Int): String =
    s"$label · $count"

  private def dayHeading(month: Int, day: Int, count: Int): String =
    val when: String =
      if site.languageSpec == Language.Russian.toSpec then
        s"$day ${russianGenitive.getOrElse(month, monthName(month))}"
      else s"${monthName(month)} $day"
    counted(when, count)

  private def monthName(month: Int): String =
    given Language.Spec = site.languageSpec
    Julian.Year(1).month(month).name.toLanguageString

  private def sortKey(row: Row): (Int, String, String, String) =
    (row.slot.day, archiveId(row.page), pathLabel(row.page), sourceOf(row.page))

  private def undatedKey(page: Page): (String, String, String) =
    (archiveId(page), pathLabel(page), sourceOf(page))

  private def archiveOf(page: Page): Option[Page] =
    (PageHeader.collectorAncestors(page) :+ page)
      .filter(_.store.isDefined)
      .dropWhile(_.path == root.path)
      .headOption

  private def archiveLabel(archive: Option[Page]): String =
    archive.map(PageHeader.pageDisplayName).filter(_.nonEmpty).getOrElse:
      given Language.Spec = site.languageSpec
      Messages.text("years.noArchive")

  private def archiveId(page: Page): String =
    val chain: Seq[Page] =
      (PageHeader.collectorAncestors(page) :+ page).filter(_.store.isDefined)
    chain.dropWhile(_.path == root.path).headOption.flatMap(_.sourcePath.map(_.fileName)).getOrElse("")

  private def pathLabel(page: Page): String =
    StoreIndexes.pathHeaderHorizontal(page, root)

  private def sourceOf(page: Page): String =
    page.sourcePath.map(_.toString).getOrElse("")

  private def linkText(page: Page): String =
    StoreIndexes.documentPath(page, root)

  private def descriptionText(page: Page): Option[String] =
    page.doc.flatMap(_.documentHeader).flatMap(_.description)
      .map(xml => PlainText.visible(xml, skipNotes = true, skipTips = false))
      .filter(_.nonEmpty)

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

  private final case class Row(page: Page, filing: TeiDate.Filing, slot: TeiDate.Slot)
  private final case class Block(day: Option[Int], rows: Seq[Row])
