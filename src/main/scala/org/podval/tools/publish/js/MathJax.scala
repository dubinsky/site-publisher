package org.podval.tools.publish.js

import zio.blocks.html.{Js, js}

object MathJax extends JSLibrary:
  val version: String = "4.1.3"

  override def imports: List[String] = List(s"tex-mml-chtml.js")

  override val inlineJs: Some[Js] = Some(js"MathJax = { tex: { inlineMath: {'[+]': [['$$', '$$']]} } };")

  override def cdn: String =
    if JSLibrary.preferCloudFlare
    then s"${JSLibrary.cloudFlare}mathjax/$version"
    else s"${JSLibrary.jsDelivr}mathjax@$version"
