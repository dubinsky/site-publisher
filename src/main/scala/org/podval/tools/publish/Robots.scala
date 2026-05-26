package org.podval.tools.publish

import org.podval.tools.publish.{Asset, Path, Site}

object Robots:
  val path: Path = Path("robots").withExtension("txt")
  
final class Robots(site: Site) extends Asset.SyntheticAsset(site, Robots.path):
  override def content: String = s"Sitemap: ${site.url}${Sitemap.path}"
