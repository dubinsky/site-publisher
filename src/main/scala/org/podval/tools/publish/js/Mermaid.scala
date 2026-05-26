package org.podval.tools.publish.js

import zio.blocks.html.*

object Mermaid extends JSLibrary:
  override def scripts: List[Dom.Element.Script] = List(
    script(`type` := "module").inlineJs(
      js"""import mermaid from '${JSLibrary.jsDelivr}/mermaid@11/dist/mermaid.esm.min.mjs';
          |mermaid.initialize({ startOnLoad: false });
          |await mermaid.run({ querySelector: '.language-mermaid', });
          |""".stripMargin
    )
  )
