package org.podval.tools.publish.site

import org.podval.tools.publish.page.{FullMarkupPage, MarkupPage, NamedWindows, Page, StoreIndexes}
import org.podval.xml.Html
import zio.blocks.html.*

final class BackLinks:
  private var backLinks: List[BackLink] = List.empty

  def addBackLinks(backLink: Seq[BackLink]): Unit = backLinks = backLinks.appendedAll(backLink)

  def html(page: MarkupPage): Option[Html.Element] =
    val pageBackLinks: Seq[(FullMarkupPage, List[BackLink])] = backLinks
      .filter(_.to.page == page)
      .filterNot(_.from == page)
      .groupBy(_.from)
      .toSeq
      .sortBy(_._1.title)

    if page.hasSyntheticContent && !page.isDirectory || pageBackLinks.isEmpty then None else Some:
      div(className := "backlinks",
        h3("Backlinks"),
        ul(groupedItems(page, pageBackLinks))
      )

  private def groupedItems(
    page: MarkupPage,
    pageBackLinks: Seq[(FullMarkupPage, List[BackLink])]
  ): Seq[Html.Element] =
    val grouped: Map[Option[Page], Seq[(FullMarkupPage, List[BackLink])]] =
      pageBackLinks.groupBy((from, _) => StoreIndexes.collectionOf(from))
    val collectionKeys: Seq[Page] = grouped.keys.flatten.toSeq
    val fromTree: Seq[Page] =
      val roots: Seq[Page] = page.site.pages.pages.filter(StoreIndexes.isRootStore)
      roots match
        case Seq(root) => StoreIndexes.collectionsUnder(root).filter(collectionKeys.contains)
        case _ => Seq.empty
    val leftover: Seq[Page] =
      collectionKeys.filterNot(fromTree.contains).sortBy(_.publishedPath.toString)
    val collectionOrder: Seq[Page] = fromTree ++ leftover
    collectionOrder.flatMap: collection =>
      grouped.get(Some(collection)).map: members =>
        collectionGroup(collection, members.sortBy(_._1.title))
    ++ grouped.get(None).toSeq.flatMap: members =>
      members.sortBy(_._1.title).map((from, links) => fromItem(from, links))

  private def collectionGroup(
    collection: Page,
    members: Seq[(FullMarkupPage, List[BackLink])]
  ): Html.Element =
    li(className := "backlinks-collection",
      a(
        className := "page-ref",
        href := collection.publishedPath.toString,
        NamedWindows.targetAttr(collection).map(name => target := name),
        StoreIndexes.collectionHeader(collection)
      ),
      ul(members.map((from, links) => fromItem(from, links)))
    )

  private def fromItem(from: FullMarkupPage, links: List[BackLink]): Html.Element =
    li(
      details(
        summary(
          from.ref(),
          span(className := "backlinks-count", links.length)
        ),
        ul(className := "backlinks-list", links.map(link =>
          val context = link.context
          li(
            a(
              href := context.url,
              NamedWindows.targetAttr(from).map(name => target := name),
              context.before,
              " ",
              span(className := "backlink", context.element),
              " ",
              context.after
            )
          )
        ))
      )
    )
