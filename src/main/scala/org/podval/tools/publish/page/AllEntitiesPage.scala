package org.podval.tools.publish.page

import org.podval.tools.publish.site.{Path, Site}
import org.podval.tools.publish.util.Icon
import org.podval.xml.Html
import zio.blocks.html.*

/** Collector `/name`: flat list of every entity. Canonical href `/name.html`; inbound `/name`. */
final class AllEntitiesPage(site: Site) extends SyntheticMarkupPage(site, AllEntitiesPage.path):
  override def titleDefault: String = "Entities"

  override protected def iconDefault: Icon = Icon.list

  override def parent: Option[DirectoryPage] = None

  override def pageHeader: Option[Html.Element] = Some(
    header(className := "post-header",
      h1(className := "post-title p-name", itemProp := "name headline", title)
    )
  )

  override def prev: Option[Page] = None
  override def next: Option[Page] = None

  override protected def syntheticContent: Html.Element =
    Page.pageList(
      site.pages.pages
        .filter(_.entityKind.isDefined)
        .sortBy(page => (page.listTitle.toLowerCase, page.titleFromPath))
    )

object AllEntitiesPage:
  val segment: String = "name"
  val path: Path = Path(segment).html
