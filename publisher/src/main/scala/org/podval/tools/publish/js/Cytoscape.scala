package org.podval.tools.publish.js

import zio.blocks.html.{Js, js}

object Cytoscape extends JSLibrary:
  val version: String = "3.34.2"
  val scriptPath: String = "/assets/js/graph.js"

  override def isModule: Boolean = true

  override def cdn: String = cdn(
    s"${JSLibrary.cloudFlare}cytoscape/$version",
    s"${JSLibrary.jsDelivr}cytoscape@$version/dist"
  )

  override def inlineJs: Some[Js] = Some:
    val cytoscape: String = s"$cdn/cytoscape.esm.min.mjs"
    js"""import cytoscape from $cytoscape;
        |import { run } from $scriptPath;
        |run(cytoscape);
        |""".stripMargin
