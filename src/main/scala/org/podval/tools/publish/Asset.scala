package org.podval.tools.publish

import org.podval.tools.publish.util.{Files, Icon, Media}
import org.podval.xml.Xml

sealed abstract class Asset(site: Site, path: Path) extends Page.Real(site: Site, path: Path):
  final override def isDirectory: Boolean = false
  final override def source: Option[MarkupPage.Source] = None
  final override def backLinks: Seq[BackLinks.BackLink] = Seq.empty
  final override def titleFromPath: String = path.fileName + path.extensionString
  override protected def iconDefault: Icon = Media.icon(path.extension).getOrElse(Icon.file)

object Asset:
  final class AssetWithSource(site: Site, source: Path, path: Path) extends Asset(site, path):
    override def sourcePath: Option[Path] = Some(source)
    override def write(): Unit = Files.copy(fromFile = path.file(site.sourceDirectory), toFile = targetFile)

  abstract class SyntheticAsset(site: Site, path: Path) extends Asset(site, path) with Page.WithContent:
    final override def sourcePath: Option[Path] = None

  abstract class SyntheticXmlAsset(site: Site, path: Path) extends SyntheticAsset(site, path):
    final override def content: String = Xml.writer.render(xmlContent)
    def xmlContent: Xml.Element
  
  final class EmbeddedAsset(site: Site, path: Path) extends SyntheticAsset(site, path):
    override def content: String = Files.readResource(resourcesBase + path.toString)

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
    Path("assets", "css", "tei").withExtension("css"),
  )
