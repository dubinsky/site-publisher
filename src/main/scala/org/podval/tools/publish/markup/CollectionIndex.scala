package org.podval.tools.publish.markup

import org.podval.tools.publish.page.{DirectoryPage, FacsimilePage, Page, PdfPage, StoreContent}
import org.podval.tools.publish.site.PageError
import org.podval.xml.Xml
import zio.blocks.chunk.Chunk

/** Collection directory index: collector `table.collection-index`, generated at render.
  * Originals are rows; `{base}-{xx}` translations are Язык links, not rows. */
object CollectionIndex:
  final class Column(
    val heading: String,
    val cssClass: String
  )

  val columns: Seq[Column] = Seq(
    Column("Описание", "description"),
    Column("Дата", "date"),
    Column("Кто", "author"),
    Column("Кому", "addressee"),
    Column("Язык", "language"),
    Column("Документ", "document"),
    Column("Страницы", "pages"),
    Column("Расшифровка", "transcriber")
  )

  def generate(page: Page, store: StoreContent): Xml.Element =
    store.pageTypeName.filterNot(PageType.isKnown).foreach: name =>
      error(page, s"unknown pageType '$name'")
    val originals: Seq[Page] = listingOriginals(page)
    val parts: Seq[(Option[Xml.Element], Seq[Page])] = splitParts(page, store.parts, originals)
    val header: Xml.Element = Xml.element("tr").setChildren(Chunk.from(columns.map: column =>
      Xml.element("th").addClass(column.cssClass).setText(column.heading): Xml.Node
    ))
    val body: Chunk[Xml.Node] = Chunk.from(parts.flatMap((title, documents) =>
      titleRow(page, title).toSeq ++ documents.map(documentRow(store, _))
    ).map(el => el: Xml.Node))
    val table: Xml.Element =
      Xml.element("table").addClass("collection-index").setChildren(header +: body)
    TeiMarkup.finishFootnotes(Xml.element("div").setChildren(table +: missingNotes(store, originals)))

  def listingChildren(store: StoreContent, children: List[Page]): List[Page] =
    if store.isCollection then children.filterNot(isTranslation) else children

  def originalsUnder(directory: DirectoryPage): List[Page] =
    directory.site.pages.pages
      .filterNot(_.isDirectory)
      .filterNot(_.isInstanceOf[PdfPage])
      .filterNot(_.isInstanceOf[FacsimilePage])
      .filter(_.path.path.init == directory.path.path.init)
      .filterNot(isTranslation)
      .sortBy(baseName)
      .toList

  def isTranslation(page: Page): Boolean = splitLang(fileName(page))._2.isDefined

  def baseName(page: Page): String = splitLang(fileName(page))._1

  def fileName(page: Page): String =
    page.sourcePath.map(_.fileName).getOrElse(page.path.fileName)

  def splitLang(name: String): (String, Option[String]) =
    val dash: Int = name.lastIndexOf('-')
    if dash == -1 || dash != name.length - 3 then (name, None)
    else (name.substring(0, dash), Some(name.substring(dash + 1)))

  def langOf(page: Page): Option[String] =
    page.doc.flatMap(_.documentHeader).flatMap(_.lang).orElse(splitLang(fileName(page))._2)

  def checkLang(sourcePathFileName: String, lang: Option[String], report: (PageError.Kind, String) => Unit): Unit =
    splitLang(sourcePathFileName)._2.foreach: fileLang =>
      if !lang.contains(fileLang) then
        report(
          PageError.FileName,
          s"Wrong language in $sourcePathFileName: ${lang.getOrElse("missing")} != $fileLang"
        )

  def translationsOf(page: Page): Seq[Page] =
    val base: String = baseName(page)
    page.parent.toSeq.flatMap: directory =>
      directory.site.pages.pages.filter: sibling =>
        sibling != page &&
        sibling.parent.contains(directory) &&
        splitLang(fileName(sibling))._2.isDefined &&
        splitLang(fileName(sibling))._1 == base
    .sortBy(sibling => langOf(sibling).getOrElse(fileName(sibling)))

  def translationsToLink(page: Page): Seq[Page] =
    if isTranslation(page) then Seq.empty
    else if !page.parent.exists(_.store.exists(_.isCollection)) then Seq.empty
    else translationsOf(page)

  private def listingOriginals(page: Page): Seq[Page] =
    page match
      case directory: DirectoryPage =>
        directory.storeChildren.getOrElse(originalsUnder(directory))
      case _ => Seq.empty

  private def documentRow(store: StoreContent, document: Page): Xml.Element =
    val header: Option[DocumentHeader] = document.doc.flatMap(_.documentHeader)
    val translations: Seq[Page] = translationsOf(document)
    val cells: Seq[Xml.Element] = Seq(
      cell("description", convertedNodes(document, header.flatMap(_.description).fold(Chunk.empty)(_.getChildren))),
      cell("date", convertedNodes(document, PageHeader.dateCell(header.flatMap(_.date)))),
      cell("author", convertedNodes(document, PageHeader.joinedInner(header.toSeq.flatMap(_.authors)))),
      cell("addressee", convertedNodes(document, Chunk.from(header.flatMap(_.addressee).toSeq.map(el => el: Xml.Node)))),
      cell("language", languageCell(document, header, translations)),
      cell("document", Chunk(documentLink(document, baseName(document)))),
      cell("pages", pagesCell(document, header, store.pageType)),
      cell("transcriber", convertedNodes(document, PageHeader.joinedInner(header.toSeq.flatMap(_.transcribers))))
    )
    Xml.element("tr").setChildren(Chunk.from(cells.map(el => el: Xml.Node)))

  private def languageCell(
    document: Page,
    header: Option[DocumentHeader],
    translations: Seq[Page]
  ): Xml.Nodes =
    val lang: Xml.Nodes = header.flatMap(_.lang).orElse(langOf(document)).fold(Chunk.empty[Xml.Node])(l => Chunk(Xml.text(l)))
    val links: Xml.Nodes = Chunk.from(translations.flatMap: translation =>
      Seq(Xml.text(" "), langLink(translation): Xml.Node)
    )
    lang ++ links

  private def pagesCell(document: Page, header: Option[DocumentHeader], pageType: PageType): Xml.Nodes =
    val links: Seq[Xml.Element] = header.toSeq.flatMap(_.pbs).map: pb =>
      Xml.element("a")
        .setHref(s"${document.publishedPath}#${Pb.pageId(pb.n)}")
        .setText(pageType.displayName(pb.n))
    links match
      case Seq() => Chunk.empty
      case Seq(one) => Chunk(one)
      case many => many.map(el => Chunk(el: Xml.Node)).reduce((left, right) => left ++ Chunk(Xml.text(" ")) ++ right)

  private def missingNotes(store: StoreContent, originals: Seq[Page]): Chunk[Xml.Node] =
    val missing: Seq[(Page, Pb)] = originals.sortBy(baseName).flatMap: document =>
      document.doc.flatMap(_.documentHeader).toSeq.flatMap(_.pbs).filter(_.isMissing).map(document -> _)
    def note(flavour: String, keep: Pb => Boolean): Option[Xml.Element] =
      val pages: Seq[String] = missing.filter((_, pb) => keep(pb)).map: (document, pb) =>
        store.pageType.displayName(pb.n)
      Option.when(pages.nonEmpty):
        Xml.element("p").setText(
          s"Отсутствуют фотографии ${pages.length} $flavour страниц: ${pages.mkString(" ")}"
        )
    Chunk.from(Seq(
      note("пустых", _.isEmpty),
      note("непустых", pb => !pb.isEmpty)
    ).flatten.map(el => el: Xml.Node))

  private def titleRow(page: Page, title: Option[Xml.Element]): Option[Xml.Element] =
    title.map: xml =>
      val inner: Xml.Element = Xml.element("span").addClass("part-title")
        .setChildren(convertedNodes(page, xml.getChildren))
      Xml.element("tr").setChildren(Chunk(
        Xml.element("td")
          .set("colspan", columns.length.toString)
          .setChildren(Chunk(inner: Xml.Node)): Xml.Node
      ))

  private def cell(cssClass: String, nodes: Xml.Nodes): Xml.Element =
    Xml.element("td").addClass(cssClass).setChildren(nodes)

  private def documentLink(document: Page, text: String): Xml.Element =
    Xml.element("a").setHref(document.publishedPath.toString).setText(text)

  private def langLink(translation: Page): Xml.Element =
    documentLink(translation, langOf(translation).getOrElse(fileName(translation)))

  private def convertedNodes(page: Page, nodes: Xml.Nodes): Xml.Nodes =
    if nodes.isEmpty then Chunk.empty
    else PageHeader.resolvedFragment(page, Xml.element("span").setChildren(nodes)).getChildren

  private def splitParts(
    page: Page,
    parts: Seq[CollectionPart],
    documents: Seq[Page]
  ): Seq[(Option[Xml.Element], Seq[Page])] =
    if parts.isEmpty then Seq((None, documents))
    else split(page, Seq.empty, parts, documents)

  @scala.annotation.tailrec
  private def split(
    page: Page,
    result: Seq[(Option[Xml.Element], Seq[Page])],
    parts: Seq[CollectionPart],
    documents: Seq[Page]
  ): Seq[(Option[Xml.Element], Seq[Page])] =
    if parts.isEmpty then
      if documents.nonEmpty then
        error(page, "Documents left over: " + documents.map(baseName).mkString(", "))
        result :+ (None, documents)
      else result
    else if documents.isEmpty then
      error(page, s"No documents for part from=${parts.head.from}")
      split(page, result, parts.tail, documents)
    else if parts.length == 1 then
      result :+ take(page, parts.head, documents)
    else
      val nextFrom: String = parts.tail.head.from
      val (partDocuments: Seq[Page], tail: Seq[Page]) = documents.span(doc => baseName(doc) != nextFrom)
      split(page, result :+ take(page, parts.head, partDocuments), parts.tail, tail)

  private def take(
    page: Page,
    part: CollectionPart,
    documents: Seq[Page]
  ): (Option[Xml.Element], Seq[Page]) =
    if documents.isEmpty then
      error(page, s"No documents for part from=${part.from}")
      (part.title, documents)
    else
      if baseName(documents.head) != part.from then
        error(page, s"Incorrect 'from' document: expected ${part.from}, got ${baseName(documents.head)}")
      (part.title, documents)

  private def error(page: Page, message: String): Unit =
    page.sourcePath.foreach: sourcePath =>
      page.site.error(sourcePath, PageError.FileName, message)
