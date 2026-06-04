package org.podval.tools.publish.page

import org.podval.tools.publish.{Path, Site}
import org.podval.tools.publish.util.Icon
import org.podval.xml.Html

final class SimpleMarkupPage(site: Site, path: Path) extends MarkupPage(site, path) with NonDirectoryPage:
  override protected def iconDefault: Icon = if isPost then Icon.envelope else Icon.note

  override def hasSyntheticContent: Boolean = false

  override protected def syntheticContentOpt: Option[Html.Element] = None
