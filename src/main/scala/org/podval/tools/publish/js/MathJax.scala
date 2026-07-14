package org.podval.tools.publish.js

import zio.blocks.html.{Js, js}

object MathJax extends JSLibrary:
  override def cdn: String = s"${JSLibrary.jsDelivr}mathjax@4"

  override def imports: List[String] = List(s"/tex-mml-chtml.js")

  override val inlineJs: Some[Js] = Some(js"MathJax = { tex: { inlineMath: {'[+]': [['$$', '$$']]} } };")
  