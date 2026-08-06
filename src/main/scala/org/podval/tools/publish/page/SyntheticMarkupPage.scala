package org.podval.tools.publish.page

import org.podval.tools.publish.site.{Path, Site}
import org.podval.xml.Html

abstract class SyntheticMarkupPage(site: Site, path: Path) extends FullMarkupPage(site, path):
  final override def hasSyntheticContent: Boolean = true

  final override protected def syntheticContentOpt: Option[Html.Element] = Some(syntheticContent)

  protected def syntheticContent: Html.Element
