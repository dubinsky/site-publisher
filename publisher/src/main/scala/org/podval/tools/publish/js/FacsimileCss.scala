package org.podval.tools.publish.js

object FacsimileCss extends JSLibrary:
  override def cdn: String = ""
  override val stylesheet: Some[String] = Some("/assets/css/facsimile.css")
