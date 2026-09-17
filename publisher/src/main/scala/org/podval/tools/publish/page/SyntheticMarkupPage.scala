package org.podval.tools.publish.page

import org.podval.tools.publish.site.{Path, Site}
import org.podval.xml.Html

abstract class SyntheticMarkupPage(site: Site, path: Path) extends MarkupPage(site, path):
  final override def hasSyntheticContent: Boolean = true

  final override protected def syntheticContentOpt: Option[Html.Element] = Some(syntheticContent)

  protected def syntheticContent: Html.Element

  override def markupContent: Option[Html.Element] = None

  override def pageHeader: Option[Html.Element] = None

  override def prev: Option[Page] = parent.flatMap(_.prev(this))
  override def next: Option[Page] = parent.flatMap(_.next(this))
