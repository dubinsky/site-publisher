package org.podval.tools.publish.page

import org.podval.tools.publish.markup.{DocumentHeader, Selector, StoreIndex, TeiMarkup}
import org.podval.tools.publish.util.Date
import org.podval.xml.{Html, Xml, XmlUtil}
import zio.blocks.html.*

object PageHeader:
  def of(page: FullMarkupPage): Html.Element =
    if isCollector(page) then collectorPageHeader(page) else pageHeader(page)

  private def isCollector(page: FullMarkupPage): Boolean =
    page.store.isDefined || collectorAncestors(page).nonEmpty

  def pageHeader(page: FullMarkupPage): Html.Element =
    header(className := "post-header",
      postPath(page),
      h1(className := "post-title p-name", itemProp := "name headline", page.title),
      Option.when(!page.hasSyntheticContent)(articleMeta(page))
    )

  private def postPath(page: Page): Html.Element =
    def parents(page: Page): Seq[Page] = page.parent match
      case None => Seq.empty
      case Some(parent) => parents(parent) :+ parent

    val pathFull: Seq[Page] = parents(page)
    val path: Seq[Page] = if pathFull.isEmpty then pathFull else pathFull.tail
    span(className := "post-path", path.map(page => span("/", page.ref(withIcon = false))))

  private def articleMeta(page: FullMarkupPage): Html.Element =
    div(className := "post-meta",
      join(
        join(
          join(
            timeHtml(Option.when(page.dateModified.nonEmpty)("Published:"), page.date, "dt-published", "datePublished"),
            "•",
            timeHtml(Some("Updated:"), page.dateModified, "dt-modified", "dateModified")
          ),
          "•",
          page.author.fold(Seq.empty): author =>
            Seq(
              span(className := "post-authors",
                span(className := "post-author", itemProp := "author", itemScope := true, itemType := "http://schema.org/Person",
                  span(className := "p-author h-card", itemProp := "name", author)
                )
              )
            )
        ),
        "|",
        page.tags.map(page.site.tags.tagRef)
      )
    )

  private def join(left: Seq[Html.Element], text: String, right: Seq[Html.Element]): Seq[Html.Element] =
    if left.nonEmpty && right.nonEmpty
    then left ++ Seq(span(className := "bullet-divider", text)) ++ right
    else left ++ right

  private def timeHtml(label: Option[String], date: Option[Date], cls: String, itemprop: String): Seq[Html.Element] =
    date.fold(Seq.empty): date =>
      label.fold(Seq.empty)(label => Seq(span(className := "meta-label", label))) ++
        Seq(time(className := cls, datetime := date.toString, itemProp := itemprop, date.toShortString))

  def collectorPageHeader(page: FullMarkupPage): Html.Element =
    XmlUtil.xml2html(collectorHeaderXml(page))

  /** Live collector: ancestor `<l>` lines, then this node's `<l>`, then abstract/body,
    * then this store's `by` selector label (the listing itself stays in the body). */
  private def collectorHeaderXml(page: FullMarkupPage): Xml.Element =
    val ancestors: Seq[Xml.Element] = collectorAncestors(page).map(ancestorLine)
    val head: Xml.Element = currentHead(page)
    val index: Option[StoreContent] = page.store
    val description: Xml.Nodes = index.flatMap(_.description).toSeq.map(xml => resolvedFragment(page, xml))
    val body: Xml.Nodes = index.flatMap(_.body).fold(Seq.empty[Xml.Node]): bodyEl =>
      resolvedFragment(page, bodyEl).getChildren
    val byLabel: Xml.Nodes = index.flatMap(_.selector).toSeq.map: selector =>
      Xml.element("l").addClass("store-by").setText(s"${Selector.displayName(selector)}:")
    val table: Xml.Nodes = documentHeaderTable(page).toSeq
    TeiMarkup.finishFootnotes(Xml.element("header").addClass("store-header").setChildren(
      ancestors.map(el => el: Xml.Node) ++
        Seq(head: Xml.Node) ++
        table ++
        description ++
        body ++
        byLabel
    ))

  private[page] def collectorAncestors(page: Page): Seq[Page] =
    def loop(opt: Option[Page]): List[Page] = opt match
      case None => Nil
      case Some(parent) =>
        val rest: List[Page] = loop(parent.parent)
        if parent.store.isDefined then rest :+ parent else rest
    loop(page.parent)

  private def ancestorLine(page: Page): Xml.Element =
    val name: Xml.Element =
      Xml.element("a").setHref(page.publishedPath.toString).setText(pageDisplayName(page))
    headingLine(
      selector = selectorName(page),
      name = Seq(name),
      title = storeTitleInner(page)
    )

  private def currentHead(page: FullMarkupPage): Xml.Element =
    val nameFromIndex: Option[Xml.Element] = page.store.flatMap: index =>
      index.names.find(_.lang.contains("ru")).orElse(index.names.headOption).map(storeNameXml)
    val name: Xml.Nodes = nameFromIndex.fold(Seq(Xml.text(pageDisplayName(page))))(n => Seq(n))
    headingLine(
      selector = selectorName(page),
      name = name,
      title = storeTitleInner(page)
    )

  private def headingLine(
    selector: Option[String],
    name: Xml.Nodes,
    title: Xml.Nodes
  ): Xml.Element =
    val sel: Xml.Nodes = selector.fold(Seq.empty[Xml.Node]): s =>
      Seq(Xml.text(Selector.displayName(s)), Xml.text(" "))
    val colon: Xml.Nodes =
      if name.nonEmpty && title.nonEmpty then Seq(Xml.text(": ")) else Seq.empty
    Xml.element("l").setChildren(sel ++ name ++ colon ++ title)

  /** `by/@selector` of the parent store, or `"document"` under a collection, or a parent
    * directory segment that is a known selector (`archive/` → архив). */
  private[page] def selectorName(page: Page): Option[String] =
    page.parent.flatMap: parent =>
      val parentIndex: Option[StoreContent] = parent.store
      parentIndex.flatMap(_.selector)
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
    Option.when(Selector.find(segment).isDefined)(segment)

  private[page] def pageDisplayName(page: Page): String =
    page.store.flatMap(_.displayName).getOrElse(page.titleFromPath)

  private def storeTitleInner(page: Page): Xml.Nodes =
    page.store.flatMap(_.title).fold(Seq.empty[Xml.Node]): title =>
      resolvedFragment(page, title).getChildren

  def resolvedFragment(page: Page, xml: Xml.Element): Xml.Element =
    val converted: Xml.Element = TeiMarkup.convertFragment(xml)
    page.content.fold(converted)(_.resolveConverted(converted))

  private def storeNameXml(name: StoreIndex.Name): Xml.Element =
    var result: Xml.Element = Xml.element("span").addClass("store-name").setText(name.n)
    name.lang.foreach(lang => result = result.set("lang", lang))
    result

  private def documentHeaderTable(page: FullMarkupPage): Option[Xml.Element] =
    val header: Option[DocumentHeader] = page.doc.flatMap(_.documentHeader)
    Option.when(header.exists(!_.isEmpty) && isCollectionDocument(page)):
      val meta: DocumentHeader = header.get
      Xml.element("table").addClass("document-header").setChildren(Seq(
        headerRow(page, "Описание", meta.description.fold(Seq.empty[Xml.Node])(_.getChildren)),
        headerRow(page, "Дата", dateCell(meta.date)),
        headerRow(page, "Кто", joinedInner(meta.authors)),
        headerRow(page, "Кому", meta.addressee.toSeq.map(el => el: Xml.Node)),
        headerRow(page, "Расшифровка", joinedInner(meta.transcribers))
      ))

  private def isCollectionDocument(page: Page): Boolean =
    page.store.isEmpty &&
      collectorAncestors(page).exists(_.store.exists(_.isCollection))

  private def headerRow(page: Page, heading: String, nodes: Xml.Nodes): Xml.Element =
    Xml.element("tr").setChildren(Seq(
      Xml.element("td").addClass("heading").setText(heading),
      Xml.element("td").addClass("value").setChildren(convertedNodes(page, nodes))
    ))

  private[page] def dateCell(date: Option[Xml.Element]): Xml.Nodes =
    date.fold(Seq.empty[Xml.Node]): el =>
      el.get("when").map(_.trim).filter(_.nonEmpty).fold(el.getChildren)(when => Seq(Xml.text(when)))

  private[page] def joinedInner(elements: Seq[Xml.Element]): Xml.Nodes =
    val inners: Seq[Xml.Nodes] = elements.map(_.getChildren)
    inners match
      case Seq() => Seq.empty
      case Seq(one) => one
      case many => many.reduce((left, right) => left ++ Seq(Xml.text(", ")) ++ right)

  private def convertedNodes(page: Page, nodes: Xml.Nodes): Xml.Nodes =
    if nodes.isEmpty then Seq.empty
    else resolvedFragment(page, Xml.element("span").setChildren(nodes)).getChildren
