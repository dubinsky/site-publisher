package org.podval.tools.publish.page

import org.podval.tools.publish.{Path, Site}

abstract class SyntheticAsset(
  site: Site, 
  path: Path
) extends Asset(
  site,
  path
) with PageWithContent
