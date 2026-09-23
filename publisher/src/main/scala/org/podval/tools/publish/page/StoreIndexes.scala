package org.podval.tools.publish.page

import org.podval.metadata.Language
import org.podval.tools.publish.site.Path
import org.podval.xml.Xml
import org.podval.xml.dsl.{*, given}

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
    val spec: Language.Spec = root.site.languageSpec
    val fromSelector: Option[String] = kind match
      case StoreIndexPage.Kind.Tree =>
        StoreTree.by(root).map(_.selector.pluralOrNames.toLanguageString(using spec))
      case StoreIndexPage.Kind.Flat =>
        Selectors.forName("case").map(_.pluralOrNames.toLanguageString(using spec))
    fromSelector
      .orElse(root.store.flatMap(_.title).map(_.getText.trim).filter(_.nonEmpty))
      .getOrElse(root.sourcePath.map(_.fileName).getOrElse(root.path.fileName))

  def tree(root: Page): Xml.Element =
    treeIndex(root)

  def flat(root: Page): Xml.Element =
    ul(collectionsUnder(root).map(flatItem(root, _)))

  private def treeIndex(storePage: Page): Xml.Element =
    val selectorLabel: String =
      StoreTree.by(storePage).map(_.selector).map(PageHeader.selectorDisplayName(_, storePage.site.languageSpec))
        .getOrElse("")
    val items: Seq[Xml.Element] = StoreTree.childPages(storePage).map(treeItem)
    div(className := "tree-index",
      ul(
        li(em(selectorLabel)),
        li(ul(items))
      )
    )

  private def treeItem(page: Page): Xml.Element =
    val nested: Seq[Xml.Element] =
      if page.store.exists(!_.isCollection) && StoreTree.childPages(page).nonEmpty
      then Seq(treeIndex(page))
      else Seq.empty
    li(treeLink(page), nested)

  private def treeLink(page: Page): Xml.Element =
    NamedWindows.setXmlTarget(
      a(href := page.publishedPath.toString, treeLabel(page)),
      page
    )

  private def treeLabel(page: Page): String =
    page.store match
      case Some(store) =>
        val name: String = page.names.doFind(page.site.languageSpec).name
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
      NamedWindows.setXmlTarget(
        a(href := collection.publishedPath.toString, label),
        collection
      )
    // Collector always emits `<abstract>`, even empty; tei.css `margin-top/bottom: 1em` is the
    // blank line between items on `/`.
    val description: Xml.Element = collection.store.flatMap(_.description).fold(Xml.element("abstract")): xml =>
      PageHeader.resolvedFragment(collection, xml)
    li(link, description)

  def pathHeaderHorizontal(page: Page, root: Page): String =
    storeChain(page).dropWhile(_.path == root.path).map(formatHeaderNode(_, page)).filter(_.nonEmpty).mkString(", ")

  /** Collection path header for backlink groups. Drops a root-store prefix when one is an ancestor. */
  def collectionHeader(collection: Page): String =
    val chain: Seq[Page] = storeChain(collection)
    val dropped: Seq[Page] =
      chain.find(isRootStore).fold(chain)(root => chain.dropWhile(_.path == root.path))
    dropped.map(formatHeaderNode(_, collection)).filter(_.nonEmpty).mkString(", ")

  def collectionOf(page: Page): Option[Page] =
    (PageHeader.collectorAncestors(page) :+ page).findLast(_.store.exists(_.isCollection))

  private def storeChain(page: Page): Seq[Page] =
    (PageHeader.collectorAncestors(page) ++ Seq(page)).filter(_.store.isDefined)

  private def formatHeaderNode(node: Page, langPage: Page): String =
    val sel: String = PageHeader.selectorName(node).map(PageHeader.selectorDisplayName(_, langPage.site.languageSpec)).getOrElse("")
    val name: String = PageHeader.pageDisplayName(node)
    s"$sel $name".trim

  def collectionsUnder(root: Page): Seq[Page] =
    def walk(page: Page): Seq[Page] =
      page.store match
        case Some(store) if store.isCollection => Seq(page)
        case Some(_) => StoreTree.childPages(page).flatMap(walk)
        case None => Seq.empty
    walk(root)
