package org.podval.tools.publish.page

import org.podval.tools.publish.util.Icon
import org.podval.tools.publish.{Path, Site}

final class Alias(
  site: Site,
  val page: Page,
  val alias: String
) extends Page(
  site,
  path = page.path.relativize(alias).html
) with NonDirectoryPage with PageWithContent:
  override def isAlias: Boolean = true

  override def real: RealPage = page.real

  override def source: Option[PageSource] = None

  override def titleDefault: String = path.fileName

  override protected def iconDefault: Icon = Icon("link", Icon.Solid)

  override def sourcePath: Option[Path] = None

  override def content: String = s"""<head><meta http-equiv="Refresh" content="0; URL=${page.real.path}"/></head>"""
