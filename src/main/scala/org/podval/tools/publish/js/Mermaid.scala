package org.podval.tools.publish.js

import zio.blocks.html.{Js, js}

object Mermaid extends JSLibrary:
  override def cdn: String = s"${JSLibrary.jsDelivr}mermaid@11/dist"
  
  override def isModule: Boolean = true

  // TODO split into `imports` and `script`
  override def inlineJs: Some[Js] = Some:
    val mermaid: String = s"$cdn/mermaid.esm.min.mjs"
    js"""import mermaid from $mermaid;
        |mermaid.initialize({ startOnLoad: false });
        |await mermaid.run({ querySelector: '.language-mermaid', });
        |""".stripMargin
