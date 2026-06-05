package org.podval.tools.publish

import org.podval.tools.publish.page.SyntheticAsset
import org.podval.tools.publish.util.Icon
import org.podval.tools.publish.{Path, Site}

object Robots:
  val path: Path = Path("robots").withExtension("txt")
  
final class Robots(site: Site) extends SyntheticAsset(site, Robots.path):
  override protected def iconDefault: Icon = Icon("robot", Icon.Solid)

  override def textContent: String = s"Sitemap: ${site.config.url}${Sitemap.path}"
