package org.podval.tools.publish.js

object Cytoscape extends JSLibrary:
  val version: String = "3.34.2"
  val scriptPath: String = "/assets/js/graph.js"

  override def isModule: Boolean = true

  override def cdn: String = cdn(
    s"${JSLibrary.cloudFlare}cytoscape/$version",
    s"${JSLibrary.jsDelivr}cytoscape@$version/dist"
  )

  override def inlineJs: Some[String] = Some:
    val cytoscape: String = Js.quote(s"$cdn/cytoscape.esm.min.mjs")
    val graph: String = Js.quote(scriptPath)
    s"""import cytoscape from $cytoscape;
       |import { run } from $graph;
       |run(cytoscape);
       |""".stripMargin
