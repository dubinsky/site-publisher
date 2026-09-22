package org.podval.tools.publish.page

import org.podval.tools.publish.markup.{EntityKind, EntityList, EntityLists as EntityListSpecs}
import org.podval.tools.publish.site.{PageError, Path}
import org.podval.xml.{Xml, XmlElement}
import Xml.given

/** Member lists for a TEI `entityLists` catalog, generated at render so the harvested
  * XML `Site.load` walks has no member hrefs (no backlinks). */
object EntityLists:
  /** URL prefix for list subpages and the Worker identity alias.
    * Directory catalog (`name/index.html`) → `name`; leaf (`lists.html`) → `lists`. */
  def prefix(catalog: Page): Seq[String] =
    if catalog.isDirectory then catalog.path.path.init
    else catalog.path.withoutHtml.path

  def listPath(catalog: Page, spec: EntityList): Path =
    Path(prefix(catalog) :+ spec.id).html

  def members(spec: EntityList, pages: Seq[Page]): Seq[Page] =
    pages
      .filter(page => page.entityKind.contains(spec.kind) && page.entityRole == spec.role)
      .sortBy(page => page.sourcePath.map(_.fileName).getOrElse(page.path.fileName))

  def kept(index: EntityListSpecs.Index, pages: Seq[Page]): Seq[(EntityList, Seq[Page])] =
    index.lists.flatMap: spec =>
      val mem: Seq[Page] = members(spec, pages)
      Option.when(mem.nonEmpty)(spec -> mem)

  /** Overall catalog: one non-empty list is that list; two or more is TOC only. */
  def generate(page: Page, index: EntityListSpecs.Index): Xml.Element =
    val keptLists: Seq[(EntityList, Seq[Page])] = kept(index, page.site.pages.pages)
    keptLists match
      case Seq((spec, mem)) =>
        listXml(spec, mem)
      case _ =>
        val children: Xml.Nodes =
          if keptLists.isEmpty then Seq.empty
          else Seq(tocXml(keptLists.map(_._1), page))
        Xml.element("entityLists").setChildren(children)

  def listPages(
    catalog: Page,
    index: EntityListSpecs.Index,
    existing: Path => Option[Page]
  ): List[EntityListPage] =
    val keptLists: Seq[(EntityList, Seq[Page])] = kept(index, catalog.site.pages.pages)
    if keptLists.length <= 1 then Nil
    else
      keptLists.toList.flatMap: (spec, mem) =>
        val path: Path = listPath(catalog, spec)
        existing(path) match
          case Some(existingPage) =>
            catalog.site.error(
              path,
              PageError.Duplicate,
              s"entity list '${spec.id}' collides with $existingPage"
            )
            None
          case None =>
            Some(EntityListPage(catalog.site, path, spec, mem))

  def listXml(spec: EntityList, members: Seq[Page]): Xml.Element =
    val lines: Xml.Nodes = members.map(member =>
      Xml.element("l").setChildren(Seq(memberLink(member, spec.kind)))
    )
    Xml.element(spec.kind.listElement).setId(spec.id).setChildren(lines)

  def displayName(page: Page): String =
    page.entityDisplayName.getOrElse(page.title)

  private def tocXml(specs: Seq[EntityList], catalog: Page): Xml.Element =
    val items: Xml.Nodes = specs.map: spec =>
      Xml.element(XmlElement.Li).setChildren(Seq(
        NamedWindows.setXmlTarget(
          Xml.element(XmlElement.A).setHref(listPath(catalog, spec).toString).setText(spec.title),
          catalog
        )
      ))
    Xml.element(XmlElement.Ul).addClass("entity-lists-toc").setChildren(items)

  private def memberLink(page: Page, kind: EntityKind): Xml.Element =
    NamedWindows.setXmlTarget(
      Xml
        .element(XmlElement.A)
        .addClass("page-ref")
        .addClass(kind.nameElement)
        .setHref(page.real.publishedPath.toString)
        .setText(displayName(page)),
      page
    )
