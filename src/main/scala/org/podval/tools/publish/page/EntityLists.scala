package org.podval.tools.publish.page

import org.podval.tools.publish.markup.{EntityKind, EntityLists as EntityListSpecs}
import org.podval.tools.publish.site.Path
import org.podval.xml.Xml
import zio.blocks.chunk.Chunk

/** Member lists for a TEI `entityLists` index, generated at render so the harvested
  * XML `Site.load` walks has no member hrefs (no backlinks). */
object EntityLists:
  val expand: String = "⇗"

  /** Built at render so the index XML `Site.load` walks has no member hrefs (no backlinks). */
  def generate(page: Page, index: EntityListSpecs.Index): Xml.Element =
    val kept: Seq[(EntityListSpecs.Spec, Seq[Page])] = index.lists.flatMap: spec =>
      val mem: Seq[Page] = members(page, spec)
      Option.when(mem.nonEmpty)(spec -> mem)
    val lists: Chunk[Xml.Node] = Chunk.from(kept.map((spec, mem) =>
      listXml(spec, mem, withHead = true, jump = Some(listPath(page, spec))): Xml.Node
    ))
    val children: Chunk[Xml.Node] =
      if kept.isEmpty then lists
      else tocXml(kept.map(_._1), page) +: lists
    Xml.element("entityLists").setChildren(children)

  def listXml(
    spec: EntityListSpecs.Spec,
    members: Seq[Page],
    withHead: Boolean,
    jump: Option[Path]
  ): Xml.Element =
    val head: Chunk[Xml.Node] =
      if !withHead then Chunk.empty
      else Chunk(Xml.element("tei-head").setChildren(headChildren(spec, jump)))
    val lines: Chunk[Xml.Node] = Chunk.from(members.map(member =>
      Xml.element("l").setChildren(Chunk(memberLink(member, spec.kind)))
    ))
    Xml.element(spec.kind.listElement).setId(spec.id).setChildren(head ++ lines)

  def entitiesUnder(indexPage: Page): Seq[Page] =
    val dir: Seq[String] = indexPage.path.path.init
    indexPage.site.pages.pages.filter: page =>
      page.entityKind.isDefined &&
      page.sourcePath.exists: sourcePath =>
        sourcePath.path.startsWith(dir) && sourcePath.path.length == dir.length + 1

  def members(indexPage: Page, spec: EntityListSpecs.Spec): Seq[Page] =
    entitiesUnder(indexPage)
      .filter(page => page.entityKind.contains(spec.kind) && page.entityRole == spec.role)
      .sortBy(page => page.sourcePath.map(_.fileName).getOrElse(page.path.fileName))

  def listPath(indexPage: Page, spec: EntityListSpecs.Spec): Path =
    Path(path = indexPage.path.path.init :+ spec.id).html

  def displayName(page: Page): String =
    page.entityDisplayName.getOrElse(page.title)

  private def tocXml(specs: Seq[EntityListSpecs.Spec], page: Page): Xml.Element =
    val items: Chunk[Xml.Node] = Chunk.from(specs.map: spec =>
      Xml.element("li").setChildren(Chunk(
        Xml.element("a").setHref(s"#${spec.id}").setText(spec.title),
        Xml.text(" "),
        Xml.element("a").setHref(listPath(page, spec).toString).setText(expand)
      ))
    )
    Xml.element("ul").addClass("entity-lists-toc").setChildren(items)

  private def headChildren(spec: EntityListSpecs.Spec, jump: Option[Path]): Chunk[Xml.Node] =
    val title: Chunk[Xml.Node] = Chunk(Xml.text(spec.title))
    jump.fold(title)(path =>
      title ++ Chunk(Xml.text(" "), Xml.element("a").setHref(path.toString).setText(expand))
    )

  private def memberLink(page: Page, kind: EntityKind): Xml.Element =
    Xml
      .element("a")
      .addClass("page-ref")
      .addClass(kind.nameElement)
      .setHref(page.real.publishedPath.toString)
      .setText(displayName(page))
