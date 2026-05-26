package org.podval.tools.publish.js

import zio.blocks.html.*

object MathJax extends JSLibrary:
  override val scripts: List[Dom.Element.Script] = List(
    // Note: `defer` here is crucial; without it, some math renders incorrectly, with `$`s visible...
    script(defer := true).externalJs(s"${JSLibrary.jsDelivr}/mathjax@4/tex-mml-chtml.js"),
    script(defer := true).inlineJs(js"MathJax = { tex: { inlineMath: {'[+]': [['$$', '$$']]} } };")
  )
  