package org.podval.tools.publish.page

import org.podval.tools.publish.site.{Path, Site}
import org.podval.tools.publish.util.Files

final class AssetWithSourcePath(site: Site, source: Path, path: Path) extends Asset(site, path):
  override def sourcePath: Option[Path] = Some(source)

  override def write(): Unit = Files.copy(fromFile = site.sourceFile(source), toFile = targetFile)
