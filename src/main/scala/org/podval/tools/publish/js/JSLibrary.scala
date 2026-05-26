package org.podval.tools.publish.js

import zio.blocks.html.Dom

abstract class JSLibrary:
  def stylesheet: Option[String] = None
  def scripts: List[Dom.Element.Script] = List.empty

object JSLibrary:
  val jsDelivr: String = "https://cdn.jsdelivr.net/npm/"
  val cloudFlare: String = "https://cdnjs.cloudflare.com/ajax/libs/"