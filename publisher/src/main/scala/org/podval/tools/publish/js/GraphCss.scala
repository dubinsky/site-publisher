package org.podval.tools.publish.js

object GraphCss extends JSLibrary:
  override def cdn: String = ""
  override val stylesheet: Some[String] = Some("/assets/css/graph.css")
