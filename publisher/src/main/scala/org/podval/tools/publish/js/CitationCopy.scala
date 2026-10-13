package org.podval.tools.publish.js

object CitationCopy extends JSLibrary:
  override def cdn: String = ""

  // A leading slash here becomes a protocol-relative URL (`//assets/...`).
  override def imports: List[String] = List("assets/js/citation.js")
