package org.podval.tools.publish.page

import org.podval.tools.publish.site.{Path, Site}
import org.podval.tools.publish.util.Icon

final class Alias(
  site: Site,
  val page: Page,
  path: Path
) extends Page(
  site,
  path
) with NonDirectoryPage with PageWithContent:
  override def isAlias: Boolean = true

  override def real: RealPage = page.real

  override def source: Option[PageSource] = None
  
  override def titleDefault: String = path.fileName

  override protected def iconDefault: Icon = Icon("link", Icon.Solid)
  
  override def textContent: String = s"""<head><meta http-equiv="Refresh" content="0; URL=${page.path}"/></head>"""

object Alias:
  def apply(site: Site, page: Page, alias: String): Alias =
    new Alias(site, page, page.path.relativize(alias).html)
