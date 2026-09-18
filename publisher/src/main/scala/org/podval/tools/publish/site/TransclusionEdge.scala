package org.podval.tools.publish.site

import org.podval.tools.publish.markup.{LinkKind, Region, Transclusion, WikiLink}
import org.podval.tools.publish.page.{FullMarkupPage, MarkupPage, NamedWindows, Page, StoreIndexes}
import org.podval.xml.{Html, Xml}
import zio.blocks.html.*
import scala.collection.mutable

final class TransclusionEdge(
  val fromPage: FullMarkupPage,
  val fromRegion: Region,
  val toPage: Option[FullMarkupPage],
  val toRegion: Option[Region],
  val href: String,
  val unresolvedFragment: Boolean,
  val nonAuthoredTarget: Boolean
)

final class TransclusionEdges:
  private var edges: List[TransclusionEdge] = List.empty
  private val cache: mutable.HashMap[(Page, Region), Option[Xml.Element]] = mutable.HashMap.empty

  def add(more: Seq[TransclusionEdge]): Unit = edges = edges.appendedAll(more)

  def cached(key: (Page, Region))(compute: => Option[Xml.Element]): Option[Xml.Element] =
    cache.getOrElseUpdate(key, compute)

  def harvest(from: FullMarkupPage): Seq[TransclusionEdge] =
    from.content.toSeq.flatMap: content =>
      content.xml.gather: element =>
        if !WikiLink.isTranscluded(element) then None
        else element.getHref.map: href =>
          val fromRegion: Region = Region.hostOf(element, content)
          val kind: Option[LinkKind] = LinkKind.of(element)
          from.site.pages.resolve(href, kind, from) match
            case None =>
              TransclusionEdge(
                fromPage = from,
                fromRegion = fromRegion,
                toPage = None,
                toRegion = None,
                href = href,
                unresolvedFragment = false,
                nonAuthoredTarget = false
              )
            case Some(link) =>
              Transclusion.authoredSource(link, href) match
                case Transclusion.Source.Ok(page, region, _) =>
                  TransclusionEdge(
                    fromPage = from,
                    fromRegion = fromRegion,
                    toPage = Some(page),
                    toRegion = Some(region),
                    href = href,
                    unresolvedFragment = false,
                    nonAuthoredTarget = false
                  )
                case Transclusion.Source.UnresolvedFragment =>
                  TransclusionEdge(
                    fromPage = from,
                    fromRegion = fromRegion,
                    toPage = None,
                    toRegion = None,
                    href = href,
                    unresolvedFragment = true,
                    nonAuthoredTarget = false
                  )
                case Transclusion.Source.NonAuthored | Transclusion.Source.MissingPage =>
                  TransclusionEdge(
                    fromPage = from,
                    fromRegion = fromRegion,
                    toPage = None,
                    toRegion = None,
                    href = href,
                    unresolvedFragment = false,
                    nonAuthoredTarget = true
                  )

  def html(page: MarkupPage): Option[Html.Element] =
    if page.hasSyntheticContent && !page.isDirectory then None
    else page.asFullMarkupPage.flatMap: full =>
      val hosts: Seq[FullMarkupPage] = edges
        .filter(_.toPage.contains(full))
        .filterNot(_.unresolvedFragment)
        .filterNot(_.nonAuthoredTarget)
        .filterNot(_.fromPage == full)
        .map(_.fromPage)
        .distinct
        .sortBy(_.title)
      if hosts.isEmpty then None
      else Some:
        div(className := "embedded-in",
          h3("Embedded in"),
          ul(grouped(page, hosts))
        )

  private def grouped(page: MarkupPage, hosts: Seq[FullMarkupPage]): Seq[Html.Element] =
    val byCollection: Map[Option[Page], Seq[FullMarkupPage]] =
      hosts.groupBy(from => StoreIndexes.collectionOf(from))
    val collectionKeys: Seq[Page] = byCollection.keys.flatten.toSeq
    val fromTree: Seq[Page] =
      val roots: Seq[Page] = page.site.pages.pages.filter(StoreIndexes.isRootStore)
      roots match
        case Seq(root) => StoreIndexes.collectionsUnder(root).filter(collectionKeys.contains)
        case _ => Seq.empty
    val leftover: Seq[Page] =
      collectionKeys.filterNot(fromTree.contains).sortBy(_.publishedPath.toString)
    val collectionOrder: Seq[Page] = fromTree ++ leftover
    collectionOrder.flatMap: collection =>
      byCollection.get(Some(collection)).map: members =>
        li(className := "backlinks-collection",
          a(
            className := "page-ref",
            href := collection.publishedPath.toString,
            NamedWindows.targetAttr(collection).map(name => target := name),
            StoreIndexes.collectionHeader(collection)
          ),
          ul(members.sortBy(_.title).map(from => li(from.ref())))
        )
    ++ byCollection.get(None).toSeq.flatMap: members =>
      members.sortBy(_.title).map(from => li(from.ref()))
