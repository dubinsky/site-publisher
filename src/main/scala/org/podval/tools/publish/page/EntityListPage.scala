package org.podval.tools.publish.page

import org.podval.tools.publish.markup.EntityLists
import org.podval.tools.publish.site.{Path, Site}
import org.podval.tools.publish.util.Icon
import org.podval.xml.{Html, Xml2Html}
import zio.blocks.html.*

final class EntityListPage(
  site: Site,
  path: Path,
  val spec: EntityLists.Spec,
  val members: Seq[Page]
) extends SyntheticMarkupPage(site, path):
  private var siblingsVar: Seq[EntityListPage] = Seq(this)

  def setSiblings(siblings: Seq[EntityListPage]): Unit = siblingsVar = siblings

  override def titleDefault: String = spec.title

  override protected def iconDefault: Icon = Icon.note

  override def pageHeader: Option[Html.Element] = Some(
    header(className := "post-header",
      h1(className := "post-title p-name", itemProp := "name headline", title)
    )
  )

  override def prev: Option[Page] = siblingsVar.takeWhile(_ != this).lastOption

  override def next: Option[Page] = siblingsVar.dropWhile(_ != this).drop(1).headOption

  override protected def syntheticContent: Html.Element =
    Xml2Html.fromXml(EntityLists.listXml(spec, members, withHead = false, jump = None))
