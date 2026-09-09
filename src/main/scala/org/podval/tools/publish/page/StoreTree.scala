package org.podval.tools.publish.page

import org.podval.metadata.{Language, Name, Names}
import org.podval.store.{Alias as StoreAlias, By, Store, Stores}
import org.podval.tools.publish.markup.StoreIndex

/** `org.podval.store` view of TEI `store`/`collection` pages after `StoreContent.bind`,
  * and of `entityLists` after `Pages.resolveEntityLists`. */
object StoreTree:
  def attach(pages: Seq[Page]): Unit =
    StoreForest(pages).attach()

  def attachEntityLists(pages: Seq[Page]): Unit =
    val listPages: Seq[EntityListPage] = pages.collect { case page: EntityListPage => page }
    pages.foreach:
      case directory: DirectoryPage =>
        directory.doc.flatMap(_.asEntityLists).foreach: lists =>
          val dir: Seq[String] = directory.path.path.init
          val byId: Map[String, EntityListPage] =
            listPages.filter(_.path.path.init == dir).map(page => page.spec.id -> page).toMap
          val ordered: Seq[EntityListPage] = lists.index.lists.flatMap(spec => byId.get(spec.id))
          lists.setTree(PageStore(directory, () => Seq(By("names", ordered.map(listStore)))))
      case _ =>

  def namesOf(page: Page): Names =
    val fromIndex: Seq[Name] = page.store.toSeq.flatMap(_.names.flatMap(toName))
    if fromIndex.nonEmpty then Names(fromIndex) else Names(nameList(page))

  def pageAt(tree: Stores[?], url: String): Option[Page] =
    try pageOf(tree.resolve(url).last)
    catch case _: IllegalArgumentException => None

  def pageOf(store: Store): Option[Page] = store match
    case node: PageStore => Some(node.page)
    case leaf: PageLeaf => Some(leaf.page)
    case _ => None

  def resolveRoots(pages: Seq[Page], siteStore: Option[Stores[?]] = None): Seq[Stores[?]] =
    pages.filter(StoreIndexes.isRootStore).flatMap(_.storeTree) ++
      pages.flatMap(_.doc.flatMap(_.asEntityLists).flatMap(_.tree)) ++
      siteStore.toSeq

  /** Site-level store: unparented TEI stores/collections, entity-list trees, and
    * standalone TEI `@alias`es. Nested aliases stay on their parent store. */
  def siteStore(pages: Seq[Page], siteNames: Names): Stores[Store] =
    val listed: Set[Page] = pages.flatMap(_.store.toSeq.flatMap(_.boundChildren)).toSet
    val unparented: Seq[Stores[?]] =
      pages.filter(page => page.store.isDefined && !listed.contains(page)).flatMap(_.storeTree)
    val entityLists: Seq[Stores[?]] =
      pages.flatMap(_.doc.flatMap(_.asEntityLists).flatMap(_.tree))
    val aliases: Seq[Store] = unparented.flatMap: node =>
      pageOf(node).flatMap(_.store).flatMap(_.alias).map: name =>
        StoreAlias(Names(name), Seq(englishName(node)))
    val kids: Seq[Store] = unparented.map(s => s: Store) ++ entityLists.map(s => s: Store) ++ aliases
    new Stores[Store]:
      override def names: Names = siteNames
      override def stores: Seq[Store] = kids

  /** Direct `Alias` children of each `Stores` node, resolved from that node. */
  def aliasPages(root: Stores[?]): Seq[(String, Page)] =
    aliasesIn(root).distinct

  private def aliasesIn(stores: Stores[?]): Seq[(String, Page)] =
    stores.stores.flatMap:
      case alias: StoreAlias =>
        pageAt(stores, "/" + alias.names.name).map(page => alias.names.name -> page).toSeq
      case child: Stores[?] => aliasesIn(child)
      case _ => Seq.empty

  private def listStore(list: EntityListPage): PageStore =
    PageStore(list, () => Seq(By("name", list.members.map(PageLeaf(_)))))

  private def englishName(store: Store): String =
    store.names.doFind(Language.English.toSpec).name

  private def nameList(page: Page): Seq[Name] =
    val pairs: Seq[(String, Language.Spec)] = page match
      case list: EntityListPage =>
        Seq(
          list.spec.id -> Language.English.toSpec,
          list.spec.title -> Language.Russian.toSpec
        )
      case _ =>
        val fileName: String = page.sourcePath.map(_.fileName).getOrElse(page.titleFromPath)
        Seq(
          fileName -> Language.Spec.empty,
          page.entityDisplayName.getOrElse("") -> Language.Spec.empty
        )
    val unique: Seq[(String, Language.Spec)] =
      pairs.map((n, spec) => n.trim -> spec).filter(_._1.nonEmpty).distinctBy(_._1)
    if unique.isEmpty then Seq(Name(page.titleFromPath, Language.Spec.empty))
    else unique.map((n, spec) => Name(n, spec))

  private def toName(name: StoreIndex.Name): Option[Name] =
    Option.when(name.n.nonEmpty):
      Name(
        name.n,
        name.lang.flatMap(Language.forDefaultName).map(_.toSpec).getOrElse(Language.Spec.empty)
      )

  private final class StoreForest(pages: Seq[Page]):
    private lazy val nodes: Map[Page, PageStore] =
      pages.flatMap(page => page.store.map(_ => page -> PageStore(page, () => childrenOf(page)))).toMap

    def attach(): Unit =
      nodes.foreach: (page, node) =>
        page.store.foreach(_.setTree(node))

    private def childrenOf(page: Page): Seq[Store] =
      val content: StoreContent = page.store.get
      val kids: Seq[Store] = content.boundChildren.map: child =>
        nodes.getOrElse(child, PageLeaf(child))
      val aliases: Seq[Store] = content.boundChildren.flatMap: child =>
        child.store.flatMap(_.alias).map: aliasName =>
          val node: Store = nodes.getOrElse(child, PageLeaf(child))
          val to: Seq[String] = content.selector.toSeq :+ englishName(node)
          StoreAlias(Names(aliasName), to)
      content.selector match
        case Some(selector) => Seq(By(selector, kids)) ++ aliases
        case None if content.isCollection => Seq(By("document", kids)) ++ aliases
        case None => kids ++ aliases

  final class PageStore(
    val page: Page,
    kids: () => Seq[Store]
  ) extends Stores[Store]:
    override def names: Names = namesOf(page)
    override lazy val stores: Seq[Store] = kids()

  final class PageLeaf(val page: Page) extends Store:
    override def names: Names = namesOf(page)
