package org.podval.tools.publish.page

import org.podval.tools.publish.util.Icon
import org.podval.tools.publish.{Path, Site}
import org.podval.xml.Html

abstract class DerivedMarkupPage(site: Site, path: Path) extends MarkupPage(site, path):
  final override def source: Option[PageSource] = None
  
  final override def titleFromPath: String = path.fileName

  final override def hasSyntheticContent: Boolean = false

  final override protected def syntheticContentOpt: Option[Html.Element] = None

  override protected def iconDefault: Icon = Icon.note // TODO page/document...
