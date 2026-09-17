package org.podval.tools.publish.js

import zio.blocks.html.{Js, js}

object MathJax extends JSLibrary:
  val version: String = "4.1.3"

  // Config must be on `window.MathJax` before the library boots, or `$...$` is ignored.
  override def inlineBeforeImports: Boolean = true

  override def imports: List[String] = List(s"tex-mml-chtml.js")

  override val inlineJs: Some[Js] = Some(js"MathJax = { tex: { inlineMath: {'[+]': [['$$', '$$']]} } };")

  override def cdn: String = cdn(
    s"${JSLibrary.cloudFlare}mathjax/$version",
    s"${JSLibrary.jsDelivr}mathjax@$version"
  )
