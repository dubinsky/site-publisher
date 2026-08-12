package org.podval.tools.publish.page

import org.podval.tools.publish.site.{Path, Site}
import org.podval.tools.publish.util.Files

final class EmbeddedAsset(site: Site, path: Path) extends SyntheticAsset(site, path):
  override def textContent: String = Files.readResource(EmbeddedAsset.resourcesBase + path.toString)

object EmbeddedAsset:
  def embeddedAssets(site: Site): List[EmbeddedAsset] = resourcesList.map(EmbeddedAsset(site, _))
  
  val mainStyleSheet: String = "/assets/css/style.css"
  
  // Note: it is not worth it writing JAR walker to "discover" six resources ;)
  private val resourcesBase: String = "/org/podval/tools/publish/site"
  private val resourcesList: List[Path] = List(
    "base",
    "initialize",
    "layout",
    "skin",
    "style",
    "tei"
  )
    .map(Path("assets", "css", _).withExtension("css"))
