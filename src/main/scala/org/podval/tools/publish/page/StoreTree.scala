package org.podval.tools.publish.page

import org.podval.metadata.{Language, Name, Names}
import org.podval.store.{Alias as StoreAlias, By, Store, Stores}
import org.podval.tools.publish.markup.StoreIndex

/** `org.podval.store` view of pages: each `Page` is a `Stores` node. */
object StoreTree:
  def namesOf(page: Page): Names =
    val fromIndex: Seq[Name] = page.store.toSeq.flatMap(_.names.flatMap(toName))
    if fromIndex.nonEmpty then Names(fromIndex) else Names(nameList(page))

  def childrenOf(page: Page): Seq[Store] =
    page match
      case list: EntityListPage =>
        Seq(By("name", list.members))
      case _ =>
        page.store match
          case Some(content) =>
            val kids: Seq[Store] = content.boundChildren
            val aliases: Seq[Store] = content.boundChildren.flatMap: child =>
              child.store.flatMap(_.alias).map: aliasName =>
                StoreAlias(Names(aliasName), content.selector.toSeq :+ englishName(child))
            content.selector match
              case Some(selector) => Seq(By(selector, kids)) ++ aliases
              case None if content.isCollection => Seq(By("document", kids)) ++ aliases
              case None => kids ++ aliases
          case None =>
            page.doc.flatMap(_.asEntityLists).map: lists =>
              val dir: Seq[String] = page.path.path.init
              val byId: Map[String, EntityListPage] =
                page.site.pages.pages.collect:
                  case p: EntityListPage if p.path.path.init == dir => p.spec.id -> p
                .toMap
              val ordered: Seq[EntityListPage] = lists.index.lists.flatMap(spec => byId.get(spec.id))
              Seq(By("names", ordered))
            .getOrElse(Seq.empty)

  def pageAt(tree: Stores[?], url: String): Option[Page] =
    tree.resolveOption(url).flatMap(path => pageOf(path.last))

  def pageOf(store: Store): Option[Page] = store match
    case page: Page => Some(page)
    case _ => None

  def resolveRoots(pages: Seq[Page], siteStore: Option[Stores[?]] = None): Seq[Stores[?]] =
    pages.filter(StoreIndexes.isRootStore) ++
      pages.filter(_.doc.exists(_.asEntityLists.isDefined)) ++
      siteStore.toSeq

  /** Site-level store: unparented TEI stores/collections, entity-list trees, and
    * standalone TEI `@alias`es. Nested aliases stay on their parent store. */
  def siteStore(pages: Seq[Page], siteNames: Names): Stores[Store] =
    val listed: Set[Page] = pages.flatMap(_.store.toSeq.flatMap(_.boundChildren)).toSet
    val unparented: Seq[Store] =
      pages.filter(page => page.store.isDefined && !listed.contains(page))
    val entityLists: Seq[Store] =
      pages.filter(_.doc.exists(_.asEntityLists.isDefined))
    val aliases: Seq[Store] = unparented.flatMap: node =>
      pageOf(node).flatMap(_.store).flatMap(_.alias).map: name =>
        StoreAlias(Names(name), Seq(englishName(node)))
    val kids: Seq[Store] = unparented ++ entityLists ++ aliases
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
