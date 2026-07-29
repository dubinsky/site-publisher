package org.podval.tools.publish.page

import org.podval.tools.publish.site.{Path, Site}
import org.podval.tools.publish.util.Files

final class EmbeddedAsset(site: Site, path: Path) extends SyntheticAsset(site, path):
  override def textContent: String = Files.readResource(EmbeddedAsset.resourcesBase + path.toString)

object EmbeddedAsset:
  def embeddedAssets(site: Site): List[EmbeddedAsset] = resourcesList.map(EmbeddedAsset(site, _))
  
  val mainStyleSheet: String = "/assets/css/style.css"
  
  // TODO list using Files.listResources
  private val resourcesBase: String = "/org/podval/tools/publish/site"
  private val resourcesList: List[Path] = List(
    Path("assets", "css", "base").withExtension("css"),
    Path("assets", "css", "initialize").withExtension("css"),
    Path("assets", "css", "layout").withExtension("css"),
    Path("assets", "css", "skin").withExtension("css"),
    Path("assets", "css", "style").withExtension("css"),
    Path("assets", "css", "asciidoc").withExtension("css"),
    Path("assets", "css", "tei").withExtension("css"),
  )
