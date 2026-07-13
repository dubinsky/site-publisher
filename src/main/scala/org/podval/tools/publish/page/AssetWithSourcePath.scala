package org.podval.tools.publish.page

import org.podval.tools.publish.util.Files
import org.podval.tools.publish.{Path, Site}

final class AssetWithSourcePath(site: Site, source: Path, path: Path) extends Asset(site, path):
  override def sourcePath: Option[Path] = Some(source)

  override def write(): Unit = Files.copy(fromFile = source.file(site.sourceDirectory), toFile = targetFile)
