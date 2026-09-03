package org.podval.tools.publish.page

import org.podval.store.Selector
import org.podval.tools.publish.site.Path
import org.podval.xml.Xml

/** Collector `Index.Tree` / `Index.Flat` for a root TEI `store`: nested archive tree and
  * a flat list of descendant collections. Generated at render so listing hrefs are not backlinks. */
object StoreIndexes:
  val collectionsSuffix: String = "collections"
  val indexSuffix: String = "index"
  val collectionsAlias: String = "collections"

  def isRootStore(page: Page): Boolean =
    page.store.exists(!_.isCollection) &&
      page.sourcePath.exists(_.path.length == 1)

  def pagePath(root: Page, suffix: String): Path =
    Path(Seq(s"${root.sourcePath.get.fileName}-$suffix")).html

  def pageTitle(root: Page, kind: StoreIndexPage.Kind): String =
    val fromSelector: Option[String] = kind match
      case StoreIndexPage.Kind.Tree =>
        root.store.flatMap(_.selector).flatMap(n => Selector.forName(n).flatMap(_.title))
      case StoreIndexPage.Kind.Flat =>
        Selector.forName("case").flatMap(_.title)
    fromSelector
      .orElse(root.store.flatMap(_.title).map(_.getText.trim).filter(_.nonEmpty))
      .getOrElse(root.sourcePath.map(_.fileName).getOrElse(root.path.fileName))

  def tree(root: Page): Xml.Element =
    treeIndex(root)

  def flat(root: Page): Xml.Element =
    val items: Xml.Nodes = collectionsUnder(root).map(flatItem(root, _))
    Xml.element("ul").setChildren(items)

  private def treeIndex(storePage: Page): Xml.Element =
    val selectorLabel: String =
      storePage.store.flatMap(_.selector).map(PageHeader.selectorDisplayName).getOrElse("")
    val items: Xml.Nodes = childrenOf(storePage).map(treeItem)
    Xml.element("div").addClass("tree-index").setChildren(Seq(
      Xml.element("ul").setChildren(Seq(
        Xml.element("li").setChildren(Seq(Xml.element("em").setText(selectorLabel))),
        Xml.element("li").setChildren(Seq(
          Xml.element("ul").setChildren(items)
        ))
      ))
    ))

  private def treeItem(page: Page): Xml.Element =
    val nested: Xml.Nodes =
      if page.store.exists(!_.isCollection) && childrenOf(page).nonEmpty
      then Seq(treeIndex(page))
      else Seq.empty
    Xml.element("li").setChildren(Seq(treeLink(page)) ++ nested)

  private def treeLink(page: Page): Xml.Element =
    Xml.element("a")
      .setHref(page.publishedPath.toString)
      .setText(treeLabel(page))

  private def treeLabel(page: Page): String =
    page.store match
      case Some(store) =>
        val name: String = store.displayName.getOrElse(page.titleFromPath)
        val title: String = store.title.map(_.getText.trim).filter(_.nonEmpty).getOrElse("")
        if title.isEmpty then s"$name:" else s"$name: $title"
      case None =>
        page.listTitle

  private def flatItem(root: Page, collection: Page): Xml.Element =
    val header: String = pathHeaderHorizontal(collection, root)
    val title: String =
      collection.store.flatMap(_.title).map(_.getText.trim).filter(_.nonEmpty)
        .getOrElse(collection.listTitle)
    val label: String = if header.isEmpty then title else s"$header: $title"
    val link: Xml.Element =
      Xml.element("a").setHref(collection.publishedPath.toString).setText(label)
    // Collector always emits `<abstract>`, even empty; tei.css `margin-top/bottom: 1em` is the
    // blank line between items on `/`.
    val description: Xml.Element = collection.store.flatMap(_.description).fold(Xml.element("abstract")): xml =>
      PageHeader.resolvedFragment(collection, xml)
    Xml.element("li").setChildren(Seq(link, description))

  def pathHeaderHorizontal(page: Page, root: Page): String =
    val chain: Seq[Page] =
      (PageHeader.collectorAncestors(page) ++ Seq(page))
        .filter(_.store.isDefined)
        .dropWhile(_.path == root.path)
    chain.map: node =>
      val sel: String = PageHeader.selectorName(node).map(PageHeader.selectorDisplayName).getOrElse("")
      val name: String = PageHeader.pageDisplayName(node)
      s"$sel $name".trim
    .filter(_.nonEmpty)
    .mkString(", ")

  def collectionsUnder(root: Page): Seq[Page] =
    def walk(page: Page): Seq[Page] =
      page.store match
        case Some(store) if store.isCollection => Seq(page)
        case Some(_) => childrenOf(page).flatMap(walk)
        case None => Seq.empty
    walk(root)

  private def childrenOf(page: Page): List[Page] =
    page.store.map(_.boundChildren).getOrElse(Nil)
