package org.podval.tools.publish.page

import org.podval.tools.publish.site.{Path, Site}
import org.podval.xml.Xml

abstract class SyntheticMarkupPage(site: Site, path: Path) extends MarkupPage(site, path):
  final override def hasSyntheticContent: Boolean = true

  final override protected def syntheticContentOpt: Option[Xml.Element] = Some(syntheticContent)

  protected def syntheticContent: Xml.Element

  override def markupContent: Option[Xml.Element] = None

  override def pageHeader: Option[Xml.Element] = None

  override def prev: Option[Page] = parent.flatMap(_.prev(this))
  override def next: Option[Page] = parent.flatMap(_.next(this))
