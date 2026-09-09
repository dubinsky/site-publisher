package org.podval.tools.publish.page

import org.podval.metadata.{Language, Name, Names}
import org.podval.store.{Alias as StoreAlias, By, Store, Stores}
import org.podval.tools.publish.markup.StoreIndex

/** `org.podval.store` view of TEI `store`/`collection` pages after `StoreContent.bind`. */
object StoreTree:
  def attach(pages: Seq[Page]): Unit =
    StoreForest(pages).attach()

  def namesOf(page: Page): Names =
    val fromIndex: Seq[Name] = page.store.toSeq.flatMap(_.names.flatMap(toName))
    if fromIndex.nonEmpty then Names(fromIndex) else Names(page.titleFromPath)

  def pageAt(tree: Stores[?], url: String): Option[Page] =
    try pageOf(tree.resolve(url).last)
    catch case _: IllegalArgumentException => None

  def pageOf(store: Store): Option[Page] = store match
    case node: PageStore => Some(node.page)
    case leaf: PageLeaf => Some(leaf.page)
    case _ => None

  private def englishName(store: Store): String =
    store.names.doFind(Language.English.toSpec).name

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
