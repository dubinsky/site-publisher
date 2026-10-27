package org.podval.tools.publish.site

import org.podval.tools.publish.markup.{PlainText, TeiDate}
import org.podval.tools.publish.page.{CollectionIndex, FullMarkupPage, GraphOwner, NamedWindows, Page,
  StoreContent, StoreIndexes, SyntheticAsset}
import org.podval.tools.publish.util.{Files, Icon, Json}
import org.podval.xml.{Xml, XmlParser}
import java.nio.charset.StandardCharsets

final class SearchJson(site: Site) extends SyntheticAsset(site, SearchJson.path):
  override protected def iconDefault: Icon = Icon("magnifying-glass", Icon.Solid)

  override def textContent: String = SearchJson.emit(site)

object SearchJson:
  val path: Path = Path("search").withExtension("json")
  val version: Int = 1
  private val warnCharacters: Int = 8000000

  private final class Record(
    val href: String,
    val title: String,
    val kind: String,
    val date: String,
    val years: Seq[Int],
    val archive: String,
    val archiveLabel: String,
    val path: String,
    val lang: String,
    val text: String,
    val target: Option[String]
  )

  def emit(site: Site): String =
    val records: Seq[Record] = site.pages.pages.flatMap(asRecord).sortBy(_.href)
    val characters: Int = records.map(_.text.length).sum
    val json: String =
      s"""{"version":$version,"records":[${records.map(recordJson).mkString(",")}]}"""
    val bytes: Int = json.getBytes(StandardCharsets.UTF_8).length
    site.log.info(s"search: ${records.size} records, $characters characters, $bytes bytes → $path")
    if characters > warnCharacters then
      site.log.warn(
        s"search text is $characters characters; exceeding $warnCharacters; the file is still written"
      )
    json

  private def asRecord(page: Page): Option[Record] =
    GraphOwner.of(page).filter(_ == page).flatMap(recordOf)

  private def recordOf(page: FullMarkupPage): Option[Record] =
    if page.sourcePath.isEmpty then None
    else if CollectionIndex.isReading(page) then None
    else if page.isDirectory && !page.hasSyntheticContent then None
    else kindOf(page) match
      case None => None
      case Some(kind) =>
        val excluded: Boolean = page.sourcePath.exists: source =>
          Path.excludedBy(page.site.config.search.excludePathPrefixes, source)
        if excluded then None
        else textOf(page, kind).map(text => build(page, kind, text))

  private def kindOf(page: FullMarkupPage): Option[String] =
    if page.doc.exists(_.publishesTei) then Some("document")
    else if page.entityKind.isDefined then Some("name")
    else if page.store.exists(_.isCollection) then Some("collection")
    else if page.store.isDefined || page.doc.exists(_.asEntityLists.isDefined) then None
    else Some("note")

  private def textOf(page: FullMarkupPage, kind: String): Option[String] = kind match
    case "document" => readXml(page).map(PlainText.diplomaticBody)
    case "name" => readXml(page).map(xml => PlainText.visible(xml, skipNotes = false, skipTips = false))
    case "note" => page.doc.map(doc => PlainText.visible(doc.xml, skipNotes = false, skipTips = true))
    case "collection" => page.store.map(collectionText)
    case _ => None

  private def readXml(page: FullMarkupPage): Option[Xml.Element] =
    page.sourcePath.flatMap: source =>
      XmlParser.parseXml(Files.read(page.site.sourceFile(source))).toOption

  private def collectionText(store: StoreContent): String =
    val title: String = store.title.fold("")(PlainText.visible(_, skipNotes = true, skipTips = false))
    val abstractText: String = store.description.fold("")(PlainText.visible(_, skipNotes = true, skipTips = false))
    if title.isEmpty then abstractText
    else if abstractText.isEmpty then title
    else s"$title\n$abstractText"

  private def build(page: FullMarkupPage, kind: String, text: String): Record =
    val (date, years) = whenOf(page, kind)
    val (archive, archiveLabel) = archiveOf(page)
    Record(
      href = page.publishedPath.toString,
      title = page.title,
      kind = kind,
      date = date,
      years = years,
      archive = archive,
      archiveLabel = archiveLabel,
      path = pathLabel(page),
      lang = langOf(page, kind),
      text = text,
      target = NamedWindows.targetAttr(page)
    )

  private def whenOf(page: FullMarkupPage, kind: String): (String, Seq[Int]) = kind match
    case "document" =>
      val filing: TeiDate.Filing = page.doc.flatMap(_.documentHeader).flatMap(_.date).fold(
        TeiDate.emptyFiling
      )(TeiDate.filingYears)
      (filing.label, filing.years)
    case "note" =>
      page.date.map(_.localDate).fold(("", Seq.empty[Int])): local =>
        (local.toString, Seq(local.getYear))
    case _ =>
      ("", Seq.empty)

  private def langOf(page: FullMarkupPage, kind: String): String = kind match
    case "document" =>
      page.doc.flatMap(_.documentHeader).flatMap(_.lang).getOrElse("")
    case "note" | "name" =>
      page.content.flatMap(_.frontMatter.lang).map(_.trim).filter(_.nonEmpty).getOrElse("")
    case _ =>
      ""

  private def archiveOf(page: Page): (String, String) =
    archivePage(page).fold(("", "")): storePage =>
      val id: String = storePage.sourcePath.map(_.fileName).getOrElse("")
      val label: String = storePage.names.doFind(storePage.site.languageSpec).name
      (id, label)

  private def pathLabel(page: Page): String =
    rootStore(page).fold(""): root =>
      StoreIndexes.documentPath(page, root)

  private def rootStore(page: Page): Option[Page] =
    (storeAncestors(page) :+ page).find(StoreIndexes.isRootStore)

  /** First store under the root. Same chain the year page uses for the archive id. */
  private def archivePage(page: Page): Option[Page] =
    rootStore(page).flatMap: root =>
      val chain: Seq[Page] = (storeAncestors(page) :+ page).filter(_.store.isDefined)
      chain.dropWhile(_.path == root.path).headOption

  private def storeAncestors(page: Page): Seq[Page] =
    def loop(opt: Option[Page]): List[Page] = opt match
      case None => Nil
      case Some(parent) =>
        val rest: List[Page] = loop(parent.parent)
        if parent.store.isDefined then rest :+ parent else rest
    loop(page.parent)

  private def recordJson(record: Record): String =
    val fields: List[String] = List(
      s""""href":${Json.string(record.href)}""",
      s""""title":${Json.string(record.title)}""",
      s""""kind":${Json.string(record.kind)}""",
      s""""date":${Json.string(record.date)}""",
      s""""years":[${record.years.mkString(",")}]""",
      s""""archive":${Json.string(record.archive)}""",
      s""""archiveLabel":${Json.string(record.archiveLabel)}""",
      s""""path":${Json.string(record.path)}""",
      s""""lang":${Json.string(record.lang)}""",
      s""""text":${Json.string(record.text)}"""
    ) ++ record.target.toList.map(name => s""""target":${Json.string(name)}""")
    fields.mkString("{", ",", "}")
