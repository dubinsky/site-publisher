package org.podval.tools.publish.page

import org.podval.tools.publish.markup.EntityList
import org.podval.tools.publish.site.{Path, Site}
import org.podval.tools.publish.util.Icon
import org.podval.xml.Xml

final class EntityListPage(
  site: Site,
  path: Path,
  val spec: EntityList,
  val members: Seq[Page]
) extends SyntheticMarkupPage(site, path):
  private var siblingsVar: Seq[EntityListPage] = Seq(this)

  def setSiblings(siblings: Seq[EntityListPage]): Unit = siblingsVar = siblings

  override def titleDefault: String = spec.title

  override protected def iconDefault: Icon = Icon.note

  override def pageHeader: Option[Xml.Element] = Some(PageHeader.of(this))

  override def prev: Option[Page] = siblingsVar.takeWhile(_ != this).lastOption

  override def next: Option[Page] = siblingsVar.dropWhile(_ != this).drop(1).headOption

  override protected def syntheticContent: Xml.Element =
    EntityLists.listXml(spec, members)
