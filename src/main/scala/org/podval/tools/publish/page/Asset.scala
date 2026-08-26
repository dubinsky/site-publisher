package org.podval.tools.publish.page

import org.podval.tools.publish.site.{Path, Site}
import org.podval.tools.publish.util.{Icon, Media}

abstract class Asset(site: Site, path: Path) extends Page(site: Site, path: Path):
  final override def titleFromPath: String = path.fileName + path.extensionString
  override protected def iconDefault: Icon = Media.icon(path.extension).getOrElse(Icon.file)
