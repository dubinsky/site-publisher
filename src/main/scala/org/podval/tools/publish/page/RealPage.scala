package org.podval.tools.publish.page

import org.podval.tools.publish.{Path, Site}

abstract class RealPage(
  site: Site,
  path: Path
) extends Page(
  site,
  path
):
  final override def isAlias: Boolean = false

  final override def real: RealPage = this
