package org.podval.tools.publish.js

object FacsimileAside extends JSLibrary:
  override def cdn: String = ""

  // A leading slash here becomes a protocol-relative URL (`//assets/...`).
  override def imports: List[String] = List("assets/js/facsimile-aside.js")
