package org.podval.tools.publish.js

import org.podval.tools.publish.util.Files
import zio.blocks.html.{Js, js}

object Cytoscape extends JSLibrary:
  val version: String = "3.34.2"

  override def isModule: Boolean = true

  override def cdn: String = cdn(
    s"${JSLibrary.cloudFlare}cytoscape/$version",
    s"${JSLibrary.jsDelivr}cytoscape@$version/dist"
  )

  override def inlineJs: Some[Js] = Some:
    val cytoscape: String = s"$cdn/cytoscape.esm.min.mjs"
    val importLine: Js = js"import cytoscape from $cytoscape;"
    val body: Js = Js(Files.readResource("/org/podval/tools/publish/site/graph.js"))
    Js(importLine.value + "\n" + body.value)
