package org.podval.tools.publish.site

import org.podval.tools.publish.page.{FullMarkupPage, GraphOwner, NamedWindows, Page, SyntheticAsset}
import org.podval.tools.publish.util.{Icon, Json}

final class GraphJson(site: Site) extends SyntheticAsset(site, GraphJson.path):
  override protected def iconDefault: Icon = Icon("share-nodes", Icon.Solid)

  override def textContent: String = GraphJson.emit(site)

object GraphJson:
  val path: Path = Path("graph").withExtension("json")
  val version: Int = 1
  private val warnNodes: Int = 1500

  private final class Edge(
    val source: String,
    val target: String,
    val kind: String
  )

  def emit(site: Site): String =
    val vertices: Seq[FullMarkupPage] = site.pages.pages.flatMap(asVertex).sortBy(idOf)
    val ids: Set[String] = vertices.map(idOf).toSet
    val edges: Seq[Edge] = (linkEdges(site, ids) ++ transcludeEdges(site, ids))
      .sortBy(e => (e.source, e.target, e.kind))
    site.log.info(s"graph: ${vertices.size} nodes, ${edges.size} edges → $path")
    if vertices.size > warnNodes then
      site.log.warn(
        s"graph has ${vertices.size} nodes; layout will be slow; " +
          "exclude-path-prefixes (source paths) or leave graph off"
      )
    s"""{"version":$version,"nodes":[${vertices.map(nodeJson).mkString(",")}],"edges":[${edges.map(edgeJson).mkString(",")}]}"""

  private def asVertex(page: Page): Option[FullMarkupPage] =
    GraphOwner.of(page).filter(_ == page).filter(include)

  private def include(page: FullMarkupPage): Boolean =
    page.sourcePath.isDefined &&
      !(page.isDirectory && !page.hasSyntheticContent) &&
      !excluded(page)

  private def excluded(page: FullMarkupPage): Boolean =
    page.sourcePath.exists: source =>
      page.site.config.graph.excludePathPrefixes.exists: prefix =>
        source.path.startsWith(Path.fromHref(prefix).path)

  private def idOf(page: FullMarkupPage): String = page.publishedPath.toString

  private def kindOf(page: FullMarkupPage): String =
    if page.entityKind.isDefined then "entity"
    else if page.doc.flatMap(_.asEntityLists).isDefined then "entity-lists"
    else if page.doc.flatMap(_.documentHeader).isDefined then "tei"
    else if page.store.isDefined then "store"
    else if page.isPost then "post"
    else if page.isDirectory then "directory"
    else "page"

  private def groupOf(page: FullMarkupPage): String =
    page.sourcePath.flatMap(_.path.headOption).getOrElse("")

  private def nodeJson(page: FullMarkupPage): String =
    val fields: List[String] = List(
      s""""id":${Json.string(idOf(page))}""",
      s""""title":${Json.string(page.title)}""",
      s""""href":${Json.string(idOf(page))}""",
      s""""kind":${Json.string(kindOf(page))}""",
      s""""group":${Json.string(groupOf(page))}""",
      s""""tags":[${page.tags.map(Json.string).mkString(",")}]"""
    ) ++ NamedWindows.targetAttr(page).toList.map(name => s""""target":${Json.string(name)}""")
    fields.mkString("{", ",", "}")

  private def edgeJson(edge: Edge): String =
    s"""{"source":${Json.string(edge.source)},"target":${Json.string(edge.target)},"kind":${Json.string(edge.kind)}}"""

  private def linkEdges(site: Site, ids: Set[String]): Seq[Edge] =
    site.backLinks.all.flatMap: link =>
      directed(link.from, link.to.page, "link", ids)
    .distinctBy(key)

  private def transcludeEdges(site: Site, ids: Set[String]): Seq[Edge] =
    if !site.config.graph.includeTransclusions then Seq.empty
    else
      site.transclusions.all.flatMap: edge =>
        if edge.unresolvedFragment || edge.nonAuthoredTarget then Seq.empty
        else edge.toPage.toSeq.flatMap(to => directed(edge.fromPage, to, "transclude", ids))
      .distinctBy(key)

  private def directed(fromPage: Page, toPage: Page, kind: String, ids: Set[String]): Option[Edge] =
    for
      from <- GraphOwner.of(fromPage)
      to <- GraphOwner.of(toPage)
      if from != to
      source = idOf(from)
      target = idOf(to)
      if ids.contains(source) && ids.contains(target)
    yield Edge(source, target, kind)

  private def key(edge: Edge): (String, String, String) = (edge.source, edge.target, edge.kind)
