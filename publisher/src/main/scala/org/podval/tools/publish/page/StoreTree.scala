package org.podval.tools.publish.page

import org.podval.metadata.{Language, Name, Names}
import org.podval.store.{Alias as StoreAlias, By, Path, Store, Stores}

/** `org.podval.store` view of pages. A `PageNode` is the store; `Page` is not. */
object StoreTree:
  /** One store per page object. `stores` stays open until `freeze` so an early read is not the snapshot. */
  final class PageNode private[StoreTree] (val page: Page) extends Stores[Store]:
    override def names: Names = page.names
    // Not a `lazy val`: the first read is before entity-list pages exist.
    private var frozen: Option[Seq[Store]] = None
    override def stores: Seq[Store] = frozen.getOrElse(StoreTree.childrenOf(page))
    def freeze(): Unit = frozen = Some(StoreTree.childrenOf(page))

  final case class ResolvedPage(
    page: Page,
    hop: Option[By[Store]],
    path: Path
  )

  /** One wrapper per page object (`eq`), not per path (`Page.equals`). */
  def node(page: Page): PageNode =
    page.nodeSlot.getOrElse:
      val created: PageNode = new PageNode(page)
      page.nodeSlot = Some(created)
      created

  /** Axis this page owns (`axes.headOption`), not the predecessor on a resolved path. */
  def by(page: Page): Option[By[Store]] = node(page).axes.headOption

  def namesOf(page: Page): Names =
    val fromIndex: Seq[Name] = page.store.toSeq.flatMap(_.names)
    if fromIndex.nonEmpty then Names(fromIndex) else Names(nameList(page))

  def childrenOf(page: Page): Seq[Store] =
    page match
      case list: EntityListPage =>
        Seq(By("name", list.members.map(node)))
      case _ =>
        page.store match
          case Some(content) =>
            val kids: Seq[Page] = content.boundChildren
            def aliases(hop: Option[Store]): Seq[StoreAlias] =
              kids.flatMap: child =>
                child.store.flatMap(_.alias).map: aliasName =>
                  val target: Path =
                    hop.fold(Path(Seq(node(child))))(h => Path(Seq(h, node(child))))
                  StoreAlias(Names(aliasName), target)
            content.selector match
              case Some(selector) =>
                val by: By[Store] = By(selector, kids.map(node))
                Seq(by) ++ aliases(Some(by))
              case None if content.isCollection =>
                val by: By[Store] = By("document", kids.map(node))
                Seq(by) ++ aliases(Some(by))
              case None =>
                kids.map(node) ++ aliases(None)
          case None =>
            page.doc.flatMap(_.asEntityLists).map: lists =>
              val prefix: Seq[String] = EntityLists.prefix(page)
              val byId: Map[String, EntityListPage] =
                page.site.pages.pages.collect:
                  case p: EntityListPage if p.path.path.init == prefix => p.spec.id -> p
                .toMap
              val ordered: Seq[EntityListPage] = lists.index.lists.flatMap(spec => byId.get(spec.id))
              // No `names` axis: list pages are the catalog's own children. Each list is `By("name")`.
              ordered.map(node)
            .getOrElse(Seq.empty)

  /** Predecessor `By` in `path.stores`, not `by(page)`. A `By` or `Alias` landing is not a page. */
  def resolvePage(root: Stores[?], url: String): Option[ResolvedPage] =
    root.resolveOption(url).flatMap: path =>
      path.last match
        case node: PageNode =>
          val hop: Option[By[Store]] =
            path.stores.dropRight(1).lastOption.collect:
              case by: By[?] => by.asInstanceOf[By[Store]]
          Some(ResolvedPage(node.page, hop, path))
        case _ => None

  def pageAt(tree: Stores[?], url: String): Option[Page] =
    resolvePage(tree, url).map(_.page)

  def pageOf(store: Store): Option[Page] = store match
    case node: PageNode => Some(node.page)
    case _ => None

  /** Pages under the page's own `By`, or direct `PageNode` children. `Alias`es are skipped. */
  def childPages(page: Page): List[Page] =
    by(page)
      .map(_.stores.flatMap(pageOf).toList)
      .getOrElse(node(page).asStores.flatMap(pageOf).toList)

  def resolveRoots(pages: Seq[Page], siteStore: Option[Stores[?]] = None): Seq[Stores[?]] =
    pages.filter(StoreIndexes.isRootStore).map(node) ++
      pages.filter(_.doc.exists(_.asEntityLists.isDefined)).map(node) ++
      siteStore.toSeq

  /** Site-level store: unparented TEI stores/collections, entity-list trees, and
    * standalone TEI `@alias`es. Nested aliases stay on their parent store. */
  def siteStore(pages: Seq[Page], siteNames: Names): Stores[Store] =
    val listed: Set[Page] = pages.flatMap(_.store.toSeq.flatMap(_.boundChildren)).toSet
    val unparented: Seq[PageNode] =
      pages.filter(page => page.store.isDefined && !listed.contains(page)).map(node)
    val entityLists: Seq[PageNode] =
      pages.filter(_.doc.exists(_.asEntityLists.isDefined)).map(node)
    val aliases: Seq[Store] = unparented.flatMap: wrapped =>
      pageOf(wrapped).flatMap(_.store).flatMap(_.alias).map: name =>
        StoreAlias(Names(name), Path(Seq(wrapped)))
    val kids: Seq[Store] = unparented ++ entityLists ++ aliases
    new Stores[Store]:
      override def names: Names = siteNames
      override def stores: Seq[Store] = kids

  /** Direct `Alias` children of each `Stores` node, resolved from that node. */
  def aliasPages(root: Stores[?]): Seq[(String, Page)] =
    aliasesIn(root).distinct

  private def aliasesIn(node: Stores[?]): Seq[(String, Page)] =
    node.storeAliases.flatMap: alias =>
      pageAt(node, "/" + alias.names.name).map(page => alias.names.name -> page)
    ++ node.asStores.flatMap:
      case child: Stores[?] => aliasesIn(child)
      case _ => Seq.empty

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
