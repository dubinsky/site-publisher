package org.podval.tools.publish.js

import zio.blocks.html.{Js, js}

object Mermaid extends JSLibrary:
  val version: String = "11.13.0"
  
  override def isModule: Boolean = true

  override def inlineJs: Some[Js] = Some:
    val mermaid: String = s"$cdn/mermaid.esm.min.mjs"
    js"""import mermaid from $mermaid;
        |mermaid.initialize({ startOnLoad: false });
        |await mermaid.run({ querySelector: '.language-mermaid', });
        |""".stripMargin

  override def cdn: String = cdn(
    s"${JSLibrary.cloudFlare}mermaid/$version",
    s"${JSLibrary.jsDelivr}mermaid@$version/dist"
  )
