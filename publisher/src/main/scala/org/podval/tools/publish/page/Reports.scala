package org.podval.tools.publish.page

import org.podval.tools.publish.markup.EntityKind
import org.podval.tools.publish.site.Site
import org.podval.xml.Xml

/** Collector report harvest: names without `@ref`, `unclear`, misnamed entity files.
  * Walks entity files, TEI documents, and store title/abstract/body — not generated indexes. */
object Reports:
  final class Hit(val text: String, val from: Page)

  final class Harvest(
    val noRefs: Seq[Hit],
    val unclears: Seq[Hit],
    val misnamed: Seq[(Page, String)]
  )

  def harvest(site: Site): Harvest =
    val pages: Seq[Page] = site.pages.pages
    val noRefs: Seq[Hit] = pages.flatMap: page =>
      trees(page).flatMap: tree =>
        namesWithoutRef(tree).map(text => Hit(text, page))
    .sortBy(_.text.toLowerCase)
    val unclears: Seq[Hit] = pages.flatMap: page =>
      trees(page).flatMap: tree =>
        unclearsIn(tree).map(text => Hit(text, page))
    .sortBy(hit => hit.from.publishedPath.toString)
    val misnamed: Seq[(Page, String)] = pages.filter(_.entityKind.isDefined).flatMap: page =>
      val id: String = page.sourcePath.map(_.fileName).getOrElse(page.titleFromPath)
      page.entityDisplayName.map(spacesToUnderscores).filter(_ != id).map(page -> _)
    .sortBy((page, _) => page.listTitle)
    Harvest(noRefs, unclears, misnamed)

  def spacesToUnderscores(what: String): String = what.replace(' ', '_')

  private def trees(page: Page): Seq[Xml.Element] =
    page.store match
      case Some(store) =>
        store.title.toSeq ++ store.description.toSeq ++ store.body.toSeq
      case None =>
        if page.entityKind.isDefined || page.doc.exists(_.documentHeader.isDefined)
        then page.content.map(_.xml).toSeq
        else Seq.empty

  private def namesWithoutRef(xml: Xml.Element): Seq[String] =
    xml.gather(
      el =>
        Option.when(
          EntityKind.forNameElement(el.getName.localName).isDefined &&
          el.get("ref").map(_.trim).forall(_.isEmpty)
        )(el.getText),
      stopAtCode = false
    ).toSeq

  private def unclearsIn(xml: Xml.Element): Seq[String] =
    xml.gather(
      el => Option.when(el.isNamed("unclear"))(el.getText),
      stopAtCode = false
    ).toSeq
