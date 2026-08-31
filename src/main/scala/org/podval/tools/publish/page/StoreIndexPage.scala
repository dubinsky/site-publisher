package org.podval.tools.publish.page

import org.podval.tools.publish.site.{Path, Site}
import org.podval.tools.publish.util.Icon
import org.podval.xml.{Html, Xml2Html}
import zio.blocks.html.*

/** Synthetic `{root}-collections.html` (tree) or `{root}-index.html` (flat collections). */
final class StoreIndexPage(
  site: Site,
  path: Path,
  val root: Page,
  val kind: StoreIndexPage.Kind
) extends SyntheticMarkupPage(site, path):
  override def titleDefault: String = StoreIndexes.pageTitle(root, kind)

  override protected def iconDefault: Icon = kind match
    case StoreIndexPage.Kind.Tree => Icon.folder
    case StoreIndexPage.Kind.Flat => Icon.list

  override def parent: Option[DirectoryPage] = None

  override def pageHeader: Option[Html.Element] = Some(
    header(className := "post-header",
      h1(className := "post-title p-name", itemProp := "name headline", title)
    )
  )

  override def prev: Option[Page] = None
  override def next: Option[Page] = None

  override protected def syntheticContent: Html.Element =
    Xml2Html.fromXml(kind match
      case StoreIndexPage.Kind.Tree => StoreIndexes.tree(root)
      case StoreIndexPage.Kind.Flat => StoreIndexes.flat(root)
    )

object StoreIndexPage:
  enum Kind derives CanEqual:
    case Tree, Flat

  def tree(site: Site, root: Page): StoreIndexPage =
    StoreIndexPage(site, StoreIndexes.pagePath(root, StoreIndexes.collectionsSuffix), root, Kind.Tree)

  def flat(site: Site, root: Page): StoreIndexPage =
    StoreIndexPage(site, StoreIndexes.pagePath(root, StoreIndexes.indexSuffix), root, Kind.Flat)
