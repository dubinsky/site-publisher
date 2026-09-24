package org.podval.tools.publish.page

import org.podval.metadata.{Language, Name}
import org.podval.store.Selector
import org.podval.tools.publish.markup.{DocumentHeader, TeiMarkup}
import org.podval.tools.publish.site.PageErrorReporter
import org.podval.tools.publish.util.Date
import org.podval.xml.{Xml, XmlAttribute, XmlElement}
import org.podval.xml.dsl.{*, given}

object PageHeader:
  def of(page: MarkupPage): Xml.Element =
    if isCollector(page) then collectorPageHeader(page) else pageHeader(page)

  private def isCollector(page: Page): Boolean =
    page.store.isDefined || page.isInstanceOf[EntityListPage] || collectorAncestors(page).nonEmpty

  def pageHeader(page: MarkupPage): Xml.Element =
    header(className := "post-header",
      postPath(page),
      h1(className := "post-title p-name", itemProp := "name headline", page.title),
      Option.when(!page.hasSyntheticContent)(articleMeta(page))
    )

  private def postPath(page: Page): Xml.Element =
    def parents(page: Page): Seq[Page] = page.parent match
      case None => Seq.empty
      case Some(parent) => parents(parent) :+ parent

    val pathFull: Seq[Page] = parents(page)
    val path: Seq[Page] = if pathFull.isEmpty then pathFull else pathFull.tail
    span(className := "post-path", path.map(page => span("/", page.ref(withIcon = false))))

  private def articleMeta(page: MarkupPage): Xml.Element =
    div(className := "post-meta",
      join(
        join(
          join(
            join(
              timeHtml(Option.when(page.dateModified.nonEmpty)("Published:"), page.date, "dt-published", "datePublished"),
              "•",
              timeHtml(Some("Updated:"), page.dateModified, "dt-modified", "dateModified")
            ),
            "•",
            page.asFullMarkupPage.flatMap(_.author).fold(Seq.empty): author =>
              Seq(
                span(className := "post-authors",
                  span(className := "post-author", itemProp := "author", itemScope := true, itemType := "http://schema.org/Person",
                    span(className := "p-author h-card", itemProp := "name", author)
                  )
                )
              )
          ),
          "|",
          page.asFullMarkupPage.toSeq.flatMap(_.tags).map(page.site.tags.tagRef)
        ),
        "|",
        page.asFullMarkupPage.toSeq.flatMap(full => page.site.categories.chips(full))
      )
    )

  private def join(left: Seq[Xml.Element], text: String, right: Seq[Xml.Element]): Seq[Xml.Element] =
    if left.nonEmpty && right.nonEmpty
    then left ++ Seq(span(className := "bullet-divider", text)) ++ right
    else left ++ right

  private def timeHtml(label: Option[String], date: Option[Date], cls: String, itemprop: String): Seq[Xml.Element] =
    date.fold(Seq.empty): date =>
      label.fold(Seq.empty)(label => Seq(span(className := "meta-label", label))) ++
        Seq(time(className := cls, datetime := date.toString, itemProp := itemprop, date.toShortString))

  def collectorPageHeader(page: MarkupPage): Xml.Element =
    collectorHeaderXml(page)

  /** Live collector: ancestor `<l>` lines, then this node's `<l>`, then abstract/body,
    * then this store's `by` selector label (the listing itself stays in the body). */
  private def collectorHeaderXml(page: MarkupPage): Xml.Element =
    val ancestors: Seq[Xml.Element] = collectorAncestors(page).map(ancestorLine)
    val head: Xml.Element = currentHead(page)
    val index: Option[StoreContent] = page.store
    val description: Xml.Nodes = index.flatMap(_.description).toSeq.map(xml => resolvedFragment(page, xml))
    val body: Xml.Nodes = index.flatMap(_.body).fold(Seq.empty[Xml.Node]): bodyEl =>
      resolvedFragment(page, bodyEl).getChildren
    val byLabel: Xml.Nodes = StoreTree.by(page).map(_.selector).toSeq.map: selector =>
      Xml.element("l").addClass("store-by").setText(s"${selectorDisplayName(selector, page.site.languageSpec)}:")
    val table: Xml.Nodes = documentHeaderTable(page).toSeq
    TeiMarkup.finishFootnotes(
      Xml.element("header").addClass("store-header").setChildren(
        ancestors.map(el => el: Xml.Node) ++
          Seq(head: Xml.Node) ++
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

  private def ancestorLine(page: Page): Xml.Element =
    val name: Xml.Element =
      NamedWindows.setXmlTarget(
        Xml.element(XmlElement.A).setHref(page.publishedPath.toString).setText(pageDisplayName(page)),
        page
      )
    headingLine(
      selector = selectorName(page),
      name = Seq(name),
      title = storeTitleInner(page),
      spec = page.site.languageSpec
    )

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
    )

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

  private def documentHeaderTable(page: MarkupPage): Option[Xml.Element] =
    val header: Option[DocumentHeader] = page.doc.flatMap(_.documentHeader)
    Option.when(header.exists(!_.isEmpty) && isCollectionDocument(page)):
      val meta: DocumentHeader = header.get
      Xml.element(XmlElement.Table).addClass("document-header").setChildren(Seq(
        headerRow(page, "Описание", meta.description.fold(Seq.empty[Xml.Node])(_.getChildren)),
        headerRow(page, "Дата", dateCell(meta.date)),
        headerRow(page, "Кто", joinedInner(meta.authors)),
        headerRow(page, "Кому", meta.addressee.toSeq.map(el => el: Xml.Node)),
        headerRow(page, "Расшифровка", joinedInner(meta.transcribers))
      ))

  private[page] def isCollectionDocument(page: Page): Boolean =
    page.store.isEmpty &&
      collectorAncestors(page).exists(_.store.exists(_.isCollection))

  private def headerRow(page: Page, heading: String, nodes: Xml.Nodes): Xml.Element =
    Xml.element(XmlElement.Tr).setChildren(Seq(
      Xml.element(XmlElement.Td).addClass("heading").setText(heading),
      Xml.element(XmlElement.Td).addClass("value").setChildren(convertedNodes(page, nodes))
    ))

  private[page] def dateCell(date: Option[Xml.Element]): Xml.Nodes =
    date.fold(Seq.empty[Xml.Node]): el =>
      def present(name: String): Option[String] =
        el.get(name).map(_.trim).filter(_.nonEmpty)
      present("when") match
        case Some(when) => Seq(el.setChildren(Seq(Xml.text(when))))
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
