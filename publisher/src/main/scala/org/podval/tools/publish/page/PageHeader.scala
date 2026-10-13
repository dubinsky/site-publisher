package org.podval.tools.publish.page

import org.podval.metadata.{Language, Name}
import org.podval.store.Selector
import org.podval.tools.publish.markup.{DocumentHeader, TeiMarkup}
import org.podval.tools.publish.site.{PageErrorReporter, Seo}
import org.podval.tools.publish.util.Date
import org.podval.xml.{Xml, XmlAttribute, XmlElement}
import org.podval.xml.dsl.{*, given}

object PageHeader:
  def of(page: MarkupPage): Xml.Element =
    if isCollector(page) then collectorPageHeader(page) else pageHeader(page)

  private def isCollector(page: Page): Boolean =
    page.store.isDefined || page.isInstanceOf[EntityListPage] || collectorAncestors(page).nonEmpty

  def pageHeader(page: MarkupPage): Xml.Element =
    val links: Seq[Xml.Node] = documentLinks(page)
    val path: Xml.Node =
      if links.isEmpty then pagePath(page)
      else pathBar(Seq(pagePath(page)), links)
    header(className := "page-header",
      path,
      h1(className := "page-title", page.title),
      Option.when(!page.hasSyntheticContent)(articleMeta(page))
    )

  private def pagePath(page: Page): Xml.Element =
    def parents(page: Page): Seq[Page] = page.parent match
      case None => Seq.empty
      case Some(parent) => parents(parent) :+ parent

    val pathFull: Seq[Page] = parents(page)
    val path: Seq[Page] = if pathFull.isEmpty then pathFull else pathFull.tail
    span(className := "page-path", path.map(page => span("/", page.ref(withIcon = false))))

  private def articleMeta(page: MarkupPage): Xml.Element =
    div(className := "page-meta",
      join(
        join(
          join(
            join(
              join(
                timeHtml(Option.when(page.dateModified.nonEmpty)("Published:"), page.date, "page-published"),
                "•",
                timeHtml(Some("Updated:"), page.dateModified, "page-modified")
              ),
              "•",
              page.asFullMarkupPage.flatMap(_.author).fold(Seq.empty): author =>
                Seq(
                  span(className := "page-authors",
                    span(className := "page-author", author)
                  )
                )
            ),
            "|",
            page.asFullMarkupPage.toSeq.flatMap(_.tags).map(page.site.tags.tagRef)
          ),
          "|",
          page.asFullMarkupPage.toSeq.flatMap(full => page.site.categories.chips(full))
        ),
        "|",
        metaSourceLink(page)
      )
    )

  private def join(left: Seq[Xml.Element], text: String, right: Seq[Xml.Element]): Seq[Xml.Element] =
    if left.nonEmpty && right.nonEmpty
    then left ++ Seq(span(className := "bullet-divider", text)) ++ right
    else left ++ right

  private def timeHtml(label: Option[String], date: Option[Date], cls: String): Seq[Xml.Element] =
    date.fold(Seq.empty): date =>
      label.fold(Seq.empty)(label => Seq(span(className := "meta-label", label))) ++
        Seq(time(className := cls, datetime := date.toString, date.toShortString))

  def collectorPageHeader(page: MarkupPage): Xml.Element =
    collectorHeaderXml(page)

  /** Ancestors are one short path. This node's `<l>` keeps the long title.
    * A `document` axis label is omitted; other `by` labels stay. The listing is in the body.
    * One root store is left off descendant paths. A collection document with no title puts its
    * crumb on that path instead of a second line. */
  private def collectorHeaderXml(page: MarkupPage): Xml.Element =
    val untitledDocument: Boolean = isCollectionDocument(page) && !hasBibliographicTitle(page)
    val head: Seq[Xml.Node] = if untitledDocument then Seq.empty else Seq(currentHead(page))
    val index: Option[StoreContent] = page.store
    val description: Xml.Nodes = index.flatMap(_.description).toSeq.map(xml => resolvedFragment(page, xml))
    val body: Xml.Nodes = index.flatMap(_.body).fold(Seq.empty[Xml.Node]): bodyEl =>
      resolvedFragment(page, bodyEl).getChildren
    val byLabel: Xml.Nodes =
      StoreTree.by(page).filterNot(_.selector.names.hasName("document")).toSeq.map: by =>
        Xml.element("l").addClass("store-by").setText(
          s"${selectorDisplayName(by.selector, page.site.languageSpec)}:"
        )
    val table: Xml.Nodes = documentHeaderTable(page).toSeq
    val crumbs: Seq[Xml.Node] = ancestorPath(
      visibleAncestors(page),
      current = Option.when(untitledDocument)(crumbLabel(page))
    ).toSeq
    val links: Seq[Xml.Node] = documentLinks(page)
    val bar: Seq[Xml.Node] =
      if crumbs.isEmpty && links.isEmpty then Seq.empty else Seq(pathBar(crumbs, links))
    TeiMarkup.finishFootnotes(
      Xml.element("header").addClass("store-header").setChildren(
        bar ++
          head ++
          table ++
          description ++
          body ++
          byLabel
      ),
      page.source.getOrElse(PageErrorReporter.Silent)
    )

  private[page] def collectorAncestors(page: Page): Seq[Page] =
    def loop(opt: Option[Page]): List[Page] = opt match
      case None => Nil
      case Some(parent) =>
        val rest: List[Page] = loop(parent.parent)
        if parent.store.isDefined then rest :+ parent else rest
    loop(page.parent)

  /** One wrapping line. A nameless store whose file name is a selector uses that selector's plural
    * (`archive` → Архивы), so the raw id is not the crumb.
    * `current` is the document itself: not a link, `aria-current="page"`. */
  private def ancestorPath(pages: Seq[Page], current: Option[String]): Option[Xml.Element] =
    val linked: Seq[Xml.Node] = pages match
      case Seq() => Seq.empty
      case head +: tail =>
        ancestorCrumb(head) +: tail.flatMap(page => Seq(pathSep: Xml.Node, ancestorCrumb(page)))
    val crumbs: Seq[Xml.Node] = current match
      case None => linked
      case Some(label) =>
        val node: Xml.Element =
          span(className := "store-path-current", aria("current") := "page", label)
        if linked.isEmpty then Seq(node) else linked ++ Seq(pathSep, node)
    Option.when(crumbs.nonEmpty)(nav(className := "store-path", aria("label") := "Path", crumbs))

  /** Drop the ancestor that is the site's only root store. A second root puts the crumb back. */
  private def visibleAncestors(page: Page): Seq[Page] =
    val ancestors: Seq[Page] = collectorAncestors(page)
    page.site.pages.pages.filter(StoreIndexes.isRootStore) match
      case Seq(root) => ancestors.filterNot(_.path == root.path)
      case _ => ancestors

  /** Nonempty `titleStmt/title`, or a markup or front-matter title. The file name is not a title. */
  private def hasBibliographicTitle(page: Page): Boolean =
    page.content.exists: content =>
      content.title.exists(_.getText.trim.nonEmpty) ||
        content.frontMatter.title.exists(_.trim.nonEmpty)

  private def pathSep: Xml.Element =
    Xml.element(XmlElement.Span).addClass("store-path-sep").set("aria-hidden", "true").setText("/")

  private def ancestorCrumb(page: Page): Xml.Element =
    NamedWindows.setXmlTarget(
      Xml.element(XmlElement.A).setHref(page.publishedPath.toString).setText(crumbLabel(page)),
      page
    )

  private def crumbLabel(page: Page): String =
    val spec: Language.Spec = page.site.languageSpec
    val short: String = pageDisplayName(page)
    val selectorId: Option[String] = selectorName(page)
    val selectorWord: Option[String] = selectorId.map(selectorDisplayName(_, spec))
    val redundant: Boolean = selectorId.exists: id =>
      Selectors.forName(id).exists: sel =>
        sel.names.hasName(short) || sel.plural.exists(_.hasName(short))
    val bareFile: Boolean =
      page.store.exists(_.names.isEmpty) && Selectors.forName(short).isDefined
    if selectorWord.isEmpty && bareFile then
      StoreTree.by(page).flatMap(_.selector.plural).map(_.toLanguageString(using spec)).getOrElse(short)
    else
      val parts: Seq[String] = selectorWord.toSeq ++ Option.unless(redundant)(short).toSeq
      if parts.nonEmpty then parts.mkString(" ") else short

  private def currentHead(page: MarkupPage): Xml.Element =
    val nameFromTree: Option[Xml.Element] = page.store.flatMap: _ =>
      val names = page.names
      names.find(page.site.languageSpec).orElse(names.names.headOption).map(storeNameXml)
    val name: Xml.Nodes = nameFromTree.fold(Seq(Xml.text(pageDisplayName(page))))(n => Seq(n))
    headingLine(
      selector = selectorName(page),
      name = name,
      title = storeTitleInner(page),
      spec = page.site.languageSpec
    ).addClass("store-current")

  private def headingLine(
    selector: Option[String],
    name: Xml.Nodes,
    title: Xml.Nodes,
    spec: Language.Spec
  ): Xml.Element =
    val sel: Xml.Nodes = selector.fold(Seq.empty[Xml.Node]): s =>
      Seq(Xml.text(selectorDisplayName(s, spec)), Xml.text(" "))
    val colon: Xml.Nodes =
      if name.nonEmpty && title.nonEmpty then Seq(Xml.text(": ")) else Seq.empty
    Xml.element("l").setChildren(sel ++ name ++ colon ++ title)

  /** `by/@selector` of the parent store, or `"document"` under a collection, or a parent
    * directory segment that is a known selector (`archive/` → archive/архив in the site language). */
  private[page] def selectorName(page: Page): Option[String] =
    page.parent.flatMap: parent =>
      val parentIndex: Option[StoreContent] = parent.store
      StoreTree.by(parent).map(_.selector).map(_.names.doFind(Language.English.toSpec).name)
        .orElse:
          Option.when(
            parentIndex.exists(_.isCollection) && page.store.isEmpty
          )("document")
        .orElse(directorySelector(parent))

  private def directorySelector(parent: Page): Option[String] =
    val segment: String =
      if parent.isDirectory && parent.path.path.length > 1
      then parent.path.path.init.last
      else parent.path.fileName
    Option.when(Selectors.forName(segment).isDefined)(segment)

  private[page] def selectorDisplayName(n: String, spec: Language.Spec): String =
    Selectors.forName(n).map(selectorDisplayName(_, spec)).getOrElse(n)

  def selectorDisplayName(selector: Selector, spec: Language.Spec): String =
    selector.toLanguageString(using spec)

  private[page] def pageDisplayName(page: Page): String =
    page.store.map(_ => page.names.doFind(page.site.languageSpec).name)
      .getOrElse:
        page match
          case _: EntityListPage => page.title
          case _ => page.titleFromPath

  private def storeTitleInner(page: Page): Xml.Nodes =
    page.store.flatMap(_.title).fold(Seq.empty[Xml.Node]): title =>
      resolvedFragment(page, title).getChildren

  def resolvedFragment(page: Page, xml: Xml.Element): Xml.Element =
    val converted: Xml.Element = TeiMarkup.convertFragment(
      xml,
      page.source.getOrElse(PageErrorReporter.Silent)
    )
    page.content.fold(converted)(_.resolveConverted(converted))

  private def storeNameXml(name: Name): Xml.Element =
    var result: Xml.Element = Xml.element(XmlElement.Span).addClass("store-name").setText(name.name)
    name.languageSpec.language.foreach(lang => result = result.set(XmlAttribute.Lang, lang.name))
    result

  /** GitHub blob link in `page-meta` for a note or entity.
    * A TEI document and a collector page carry it in the utilities row instead. */
  private def metaSourceLink(page: MarkupPage): Seq[Xml.Element] =
    if page.doc.exists(_.documentHeader.isDefined) || isCollector(page) then Seq.empty
    else sourceLink(page).toSeq

  /** Crumbs on the left. TEI, GitHub, and Cite on the right. */
  private def pathBar(crumbs: Seq[Xml.Node], links: Seq[Xml.Node]): Xml.Element =
    div(className := "store-path-bar", crumbs ++ links)

  /** TEI and GitHub, then Cite on a TEI document. The citation text is not shown. */
  private def documentLinks(page: MarkupPage): Seq[Xml.Node] =
    val items: Seq[Xml.Element] = teiLink(page).toSeq ++ headerSourceLink(page).toSeq ++ copyButton(page).toSeq
    if items.isEmpty then Seq.empty
    else Seq(p(className := "document-links", intersperse(items)))

  private def intersperse(links: Seq[Xml.Element]): Seq[Xml.Node] =
    links match
      case Seq() => Seq.empty
      case head +: tail =>
        Seq(head) ++ tail.flatMap(link => Seq(span(className := "bullet-divider", "·"): Xml.Node, link))

  /** Utilities row: a collector page, or a TEI document (including one that is not under a store). */
  private def headerSourceLink(page: MarkupPage): Option[Xml.Element] =
    if page.doc.exists(_.documentHeader.isDefined) || isCollector(page) then sourceLink(page) else None

  private def teiLink(page: Page): Option[Xml.Element] =
    Option.when(page.doc.exists(_.documentHeader.isDefined))(
      a(
        href := page.publishedPath.withExtension(TeiDocumentPage.extension).toString,
        titleAttr := "Published TEI",
        "TEI"
      )
    )

  private def sourceLink(page: Page): Option[Xml.Element] =
    page.sourcePath.map(_.toString).flatMap(page.site.git.blobUrl).map: url =>
      a(className := "source-link", href := url, titleAttr := "Source file on GitHub", "GitHub")

  private def copyButton(page: MarkupPage): Option[Xml.Element] =
    Option.when(page.doc.exists(_.documentHeader.isDefined))(
      Xml.element("button").addClass("citation-copy").set("type", "button")
        .set(XmlAttribute.Title, "Copy citation")
        .set("data-citation", clipboardCitation(page))
        .setText("Cite")
    )

  /** Clipboard text. Not rendered. No access date.
    * The git clause is the full HEAD SHA and the file URL, the same URL as the GitHub link. */
  private def clipboardCitation(page: Page): String =
    val sentence: String = citationLine(page)
    val hash: Option[String] = page.site.git.head.map(head => s"Git hash ${head.sha}.")
    val file: Option[String] = page.sourcePath.map(_.toString).flatMap(page.site.git.blobUrl)
    val tail: String = (hash.toSeq ++ file.toSeq).mkString(" ")
    if sentence.nonEmpty && tail.nonEmpty then s"$sentence $tail"
    else if sentence.nonEmpty then sentence
    else tail

  /** Archival address, canonical URL, transcriber, and licence. */
  private def citationLine(page: Page): String =
    val transcriber: Option[String] = page.doc.flatMap(_.documentHeader)
      .map(_.transcribers)
      .map(plainText)
      .filter(_.nonEmpty)
      .map(names => s"Расшифровка: $names")
    val parts: Seq[String] = Seq(
      Option(addressLabel(page)).filter(_.nonEmpty),
      Some(Seo.canonical(page)),
      transcriber,
      licenseClause(page)
    ).flatten
    if parts.isEmpty then "" else parts.mkString(". ") + "."

  /** Store crumbs, with the single root store omitted, then this document. */
  private[page] def addressLabel(page: Page): String =
    val self: Seq[Page] = if page.store.isEmpty then Seq(page) else Seq.empty
    (visibleAncestors(page) ++ self).map(crumbLabel).filter(_.nonEmpty).mkString(", ")

  private def licenseClause(page: Page): Option[String] =
    page.site.config.license.map: license =>
      val name: String = license.name.trim
      license.holder.map(_.trim).filter(_.nonEmpty) match
        case Some(holder) => s"$name, $holder"
        case None => name

  private def plainText(elements: Seq[Xml.Element]): String =
    elements.map(_.getText.trim).filter(_.nonEmpty).mkString(", ")

  private def documentHeaderTable(page: MarkupPage): Option[Xml.Element] =
    page.doc.flatMap(_.documentHeader).filter(meta => !meta.isEmpty && isCollectionDocument(page)).flatMap: meta =>
      val rows: Seq[Xml.Element] = Seq(
        headerRow(page, "Описание", meta.description.fold(Seq.empty[Xml.Node])(_.getChildren)),
        headerRow(page, "Дата", dateCell(meta.date)),
        headerRow(page, "Кто", joinedInner(meta.authors)),
        headerRow(page, "Кому", meta.addressee.toSeq.map(el => el: Xml.Node)),
        headerRow(page, "Расшифровка", joinedInner(meta.transcribers)),
        headerRow(page, "Перевод", joinedInner(meta.translators))
      ).flatten
      Option.when(rows.nonEmpty)(
        Xml.element(XmlElement.Table).addClass("document-header").setChildren(rows)
      )

  private[page] def isCollectionDocument(page: Page): Boolean =
    page.store.isEmpty &&
      collectorAncestors(page).exists(_.store.exists(_.isCollection))

  /** A line whose converted value has no text is omitted. `?` and other authored text stay. */
  private def headerRow(page: Page, heading: String, nodes: Xml.Nodes): Option[Xml.Element] =
    val value: Xml.Nodes = convertedNodes(page, nodes)
    Option.when(hasVisibleContent(value))(
      Xml.element(XmlElement.Tr).setChildren(Seq(
        Xml.element(XmlElement.Td).addClass("heading").setText(heading),
        Xml.element(XmlElement.Td).addClass("value").setChildren(value)
      ))
    )

  private def hasVisibleContent(nodes: Xml.Nodes): Boolean =
    nodes.exists(hasVisibleNode)

  private def hasVisibleNode(node: Xml.Node): Boolean =
    node.asText.exists(_.trim.nonEmpty) ||
      node.asElement.exists(_.getChildren.exists(hasVisibleNode))

  private[page] def dateCell(date: Option[Xml.Element]): Xml.Nodes =
    date.fold(Seq.empty[Xml.Node]): el =>
      def present(name: String): Option[String] =
        el.get(name).map(_.trim).filter(_.nonEmpty)
      val notes: Xml.Nodes = el.childElements.filter(_.isNamed("note")).toSeq
      present("when") match
        case Some(when) => Seq(el.setChildren(Seq(Xml.text(when)) ++ notes))
        case None if Seq("from", "to", "notBefore", "notAfter").exists(name => present(name).isDefined) =>
          Seq(el)
        case None => el.getChildren

  private[page] def joinedInner(elements: Seq[Xml.Element]): Xml.Nodes =
    val inners: Seq[Xml.Nodes] = elements.map(_.getChildren)
    inners match
      case Seq() => Seq.empty
      case Seq(one) => one
      case many => many.reduce((left, right) => left ++ Seq(Xml.text(", ")) ++ right)

  private def convertedNodes(page: Page, nodes: Xml.Nodes): Xml.Nodes =
    if nodes.isEmpty then Seq.empty
    else resolvedFragment(page, Xml.element(XmlElement.Span).setChildren(nodes)).getChildren
