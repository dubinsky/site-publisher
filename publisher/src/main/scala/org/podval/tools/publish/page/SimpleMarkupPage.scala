package org.podval.tools.publish.page

import org.podval.tools.publish.site.{Path, Site}
import org.podval.tools.publish.util.Icon

final class SimpleMarkupPage(site: Site, path: Path) extends FullMarkupPage(site, path):
  override protected def iconDefault: Icon = if isPost then Icon.envelope else Icon.note
