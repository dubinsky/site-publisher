package org.podval.tools.publish.page

import org.podval.metadata.{Language, TermCatalog}
import org.podval.tools.publish.markup.{CollectionPart, DocumentHeader, PageType, Pb, TeiMarkup}
import org.podval.tools.publish.site.{Messages, PageError, PageErrorReporter, Path, Site}
import org.podval.xml.{Xml, XmlElement}

/** Collection directory index: collector `table.collection-index`, generated at render.
  * Originals are rows; `{base}-{xx}` translations are Язык links, not rows. */
object CollectionIndex:
  final class Column(
    val heading: String,
    val cssClass: String
  )

  private def columns(terms: TermCatalog)(using Language.Spec): Seq[Column] = Seq(
    Column(Messages.text("doc.description"), "description"),
    Column(Messages.text("doc.date"), "date"),
    Column(Messages.text("doc.who"), "author"),
    Column(Messages.text("doc.to"), "addressee"),
    Column(terms.heading("language"), "language"),
    Column(terms.heading("document"), "document"),
    Column(terms.pluralHeading("page"), "pages"),
    Column(Messages.text("doc.transcriber"), "transcriber")
  )

  def generate(page: Page, store: StoreContent): Xml.Element =
    given Language.Spec = page.site.languageSpec
    store.pageTypeName.filterNot(PageType.isKnown).foreach: name =>
      error(page, s"unknown pageType '$name'")
    val originals: Seq[Page] = listingOriginals(page)
    val parts: Seq[(Option[Xml.Element], Seq[Page])] = splitParts(page, store.parts, originals)
    val header: Xml.Element = Xml.element(XmlElement.Tr).setChildren(columns(page.site.terms).map: column =>
      Xml.element(XmlElement.Th).addClass(column.cssClass).setText(column.heading): Xml.Node
    )
    val body: Xml.Nodes = parts.flatMap((title, documents) =>
      titleRow(page, title).toSeq ++ documents.map(documentRow(store, _))
    ).map(el => el: Xml.Node)
    val table: Xml.Element =
      Xml.element(XmlElement.Table).addClass("collection-index").setChildren(header +: body)
    val scroll: Xml.Element =
      Xml.element(XmlElement.Div).addClass("collection-scroll").setChildren(Seq(table))
    TeiMarkup.finishFootnotes(
      Xml.element(XmlElement.Div).setChildren(counts(store, originals) :+ scroll),
      page.source.getOrElse(PageErrorReporter.Silent)
    )

  val readingSuffix: String = "+reg"

  def listingChildren(store: StoreContent, children: List[Page]): List[Page] =
    if store.isCollection then children.filterNot(page => isTranslation(page) || isReading(page)) else children

  def originalsUnder(directory: DirectoryPage): List[Page] =
    directory.site.pages.pages
      .filterNot(_.isDirectory)
      .filterNot(_.isInstanceOf[PdfPage])
      .filterNot(_.isInstanceOf[TeiDocumentPage])
      .filterNot(_.isInstanceOf[FacsimilePage])
      .filter(_.path.path.init == directory.path.path.init)
      .filterNot(isTranslation)
      .filterNot(isReading)
      .sortBy(baseName)

  def isTranslation(page: Page): Boolean = splitLang(fileName(page))._2.isDefined

  def isReadingName(name: String): Boolean = name.endsWith(readingSuffix)

  def isReading(page: Page): Boolean = isReadingName(fileName(page))

  /** Stem the reading transcribes. `doc-ru+reg` is `doc-ru`, which may itself be a translation. */
  def withoutReading(name: String): String =
    if isReadingName(name) then name.dropRight(readingSuffix.length) else name

  def baseName(page: Page): String = splitLang(fileName(page))._1

  def fileName(page: Page): String =
    page.sourcePath.map(_.fileName).getOrElse(page.path.fileName)

  /** `+reg` is not a language. The dash rule does not see that stem, even when the name under it would match. */
  def splitLang(name: String): (String, Option[String]) =
    if isReadingName(name) then (name, None)
    else
      val dash: Int = name.lastIndexOf('-')
      if dash == -1 || dash != name.length - 3 then (name, None)
      else (name.substring(0, dash), Some(name.substring(dash + 1)))

  /** The `+reg` sibling of this page, same extension and parent. A reading has none. */
  def readingOf(page: Page): Option[MarkupPage] =
    page.sourcePath.filter(source => !isReadingName(source.fileName)).flatMap: source =>
      val readingPath: Path = Path(source.path.init :+ (source.fileName + readingSuffix), source.extension)
      page.site.pages.pages.collectFirst:
        case candidate: MarkupPage if candidate.sourcePath.contains(readingPath) => candidate

  /** The sibling this reading transcribes. Not the translation's unsuffixed original. */
  def originalOfReading(page: Page): Option[Page] =
    page.sourcePath.filter(source => isReadingName(source.fileName)).flatMap: source =>
      val originalPath: Path = Path(source.path.init :+ withoutReading(source.fileName), source.extension)
      page.site.pages.pages.find(_.sourcePath.contains(originalPath))

  /** After the scan. The reading's file code, before the site fallback, must equal the original's. */
  def reportReadings(site: Site): Unit =
    site.pages.pages.foreach:
      case reading: MarkupPage if isReading(reading) =>
        reading.sourcePath.foreach: source =>
          val _ = reading.lang
          originalOfReading(reading) match
            case Some(original: MarkupPage) =>
              val _ = original.lang
              reportReadingLanguage(site, reading, original, source)
            case _ =>
              site.error(source, PageError.FileName, s"no original for ${sourceFile(source)}")
      case _ =>

  private def reportReadingLanguage(
    site: Site,
    reading: MarkupPage,
    original: MarkupPage,
    source: Path
  ): Unit =
    if !reading.source.exists(_.languageDisagrees) then
      val left: Option[String] = reading.source.flatMap(_.fileDocumentCode)
      val right: Option[String] = original.source.flatMap(_.fileDocumentCode)
      if left != right then
        site.error(
          source,
          PageError.FileName,
          s"Wrong language in ${sourceFile(source)}: ${left.getOrElse("")} != ${right.getOrElse("")}"
        )

  private def sourceFile(source: Path): String =
    source.fileName + source.extension.fold("")("." + _)

  def langOf(page: Page): Option[String] =
    page.doc.flatMap(_.documentHeader).flatMap(_.lang).orElse(splitLang(fileName(page))._2)

  private def listingOriginals(page: Page): Seq[Page] =
    page match
      case directory: DirectoryPage =>
        directory.storeChildren.getOrElse(originalsUnder(directory))
      case _ => Seq.empty

  private def documentRow(store: StoreContent, document: Page): Xml.Element =
    val header: Option[DocumentHeader] = document.doc.flatMap(_.documentHeader)
    val cells: Seq[Xml.Element] = Seq(
      cell("description", convertedNodes(document, header.flatMap(_.description).fold(Seq.empty)(_.getChildren))),
      cell("date", convertedNodes(document, PageHeader.dateCell(header.flatMap(_.date)))),
      cell("author", convertedNodes(document, PageHeader.joinedInner(header.toSeq.flatMap(_.authors)))),
      cell("addressee", convertedNodes(document, header.flatMap(_.addressee).toSeq.map(el => el: Xml.Node))),
      cell("language", languageCell(document, header)),
      cell("document", Seq(documentLink(document, baseName(document)))),
      cell("pages", pagesCell(document, header, store.pageType)),
      cell("transcriber", convertedNodes(document, PageHeader.joinedInner(header.toSeq.flatMap(_.transcribers))))
    )
    Xml.element(XmlElement.Tr).setChildren(cells.map(el => el: Xml.Node))

  private def languageCell(document: Page, header: Option[DocumentHeader]): Xml.Nodes =
    val lang: Xml.Nodes = header.flatMap(_.lang).orElse(langOf(document)).fold(Seq.empty[Xml.Node])(l => Seq(Xml.text(l)))
    val links: Xml.Nodes = document.site.pages.alternates.others(document).flatMap: member =>
      Seq(Xml.text(" "), langLink(member.page, member.code): Xml.Node)
    lang ++ links

  private def pagesCell(document: Page, header: Option[DocumentHeader], pageType: PageType): Xml.Nodes =
    val groups: Seq[Xml.Nodes] = header.toSeq.flatMap(_.pbs).map: pb =>
      Seq(
        NamedWindows.setXmlTarget(
          Xml.element(XmlElement.A)
            .setHref(s"${document.publishedPath}#${Pb.pageId(pb.n)}")
            .setText(pageType.displayName(pb.n)),
          document
        ): Xml.Node
      )
    groups match
      case Seq() => Seq.empty
      case Seq(one) => one
      case many => many.reduce((left, right) => left ++ Seq(Xml.text(" ")) ++ right)

  /** Document total and one closed disclosure per non-zero leaf count, on one line above the table. */
  private def counts(store: StoreContent, originals: Seq[Page])(using Language.Spec): Seq[Xml.Node] =
    val ordered: Seq[Page] = originals.sortBy(baseName)
    val headers: Seq[(Page, DocumentHeader)] = ordered.flatMap: document =>
      document.doc.flatMap(_.documentHeader).map(document -> _)
    val missing: Seq[(Page, Pb)] = headers.flatMap: (document, header) =>
      header.pbs.filter(_.isMissing).map(document -> _)
    val untranscribed: Seq[(Page, Pb)] = headers.flatMap: (document, header) =>
      if header.hasBodyText then Seq.empty
      else header.pbs.filter(pb => !pb.isMissing).map(document -> _)
    def leaves(keep: Pb => Boolean): Seq[Xml.Element] =
      missing.filter((_, pb) => keep(pb)).map((document, pb) => leafLink(store, document, pb))
    val gaps: Seq[Xml.Element] = Seq(
      leafDisclosure(Messages.text("collection.missingEmpty", "n" -> leaves(_.isEmpty).length), leaves(_.isEmpty)),
      leafDisclosure(
        Messages.text("collection.missingText", "n" -> leaves(pb => !pb.isEmpty).length),
        leaves(pb => !pb.isEmpty)
      ),
      leafDisclosure(
        Messages.text("collection.untranscribed", "n" -> untranscribed.length),
        untranscribed.map((document, pb) => leafLink(store, document, pb))
      )
    ).flatten
    Seq(countLine(ordered.length, gaps))

  private def countLine(documents: Int, gaps: Seq[Xml.Element])(using Language.Spec): Xml.Element =
    Xml.element(XmlElement.Div).addClass("collection-counts").setChildren(
      (Xml.text(Messages.text("collection.counts", "documents" -> documents)): Xml.Node) +:
        gaps.map(gap => gap: Xml.Node)
    )

  private def leafDisclosure(summary: String, links: Seq[Xml.Element]): Option[Xml.Element] =
    Option.when(links.nonEmpty):
      Xml.element(XmlElement.Details).addClass("collection-gap").setChildren(Seq(
        Xml.element(XmlElement.Summary).setText(summary),
        Xml.element(XmlElement.P).setChildren(spaced(links))
      ))

  private def leafLink(store: StoreContent, document: Page, pb: Pb): Xml.Element =
    NamedWindows.setXmlTarget(
      Xml.element(XmlElement.A)
        .setHref(s"${document.publishedPath}#${Pb.pageId(pb.n)}")
        .setText(store.pageType.displayName(pb.n)),
      document
    )

  private def spaced(nodes: Seq[Xml.Element]): Xml.Nodes =
    nodes match
      case Seq() => Seq.empty
      case head +: tail => (head: Xml.Node) +: tail.flatMap(node => Seq(Xml.text(" "), node: Xml.Node))

  private def titleRow(page: Page, title: Option[Xml.Element])(using Language.Spec): Option[Xml.Element] =
    title.map: xml =>
      val inner: Xml.Element = Xml.element(XmlElement.Span).addClass("part-title")
        .setChildren(convertedNodes(page, xml.getChildren))
      Xml.element(XmlElement.Tr).setChildren(Seq(
        Xml.element(XmlElement.Td)
          .set("colspan", columns(page.site.terms).length.toString)
          .setChildren(Seq(inner: Xml.Node)): Xml.Node
      ))

  private def cell(cssClass: String, nodes: Xml.Nodes): Xml.Element =
    Xml.element(XmlElement.Td).addClass(cssClass).setChildren(nodes)

  private def documentLink(document: Page, text: String): Xml.Element =
    NamedWindows.setXmlTarget(
      Xml.element(XmlElement.A).setHref(document.publishedPath.toString).setText(text),
      document
    )

  private def langLink(translation: Page, code: String): Xml.Element =
    documentLink(translation, code)

  private def convertedNodes(page: Page, nodes: Xml.Nodes): Xml.Nodes =
    if nodes.isEmpty then Seq.empty
    else PageHeader.resolvedFragment(page, Xml.element(XmlElement.Span).setChildren(nodes)).getChildren

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
