package org.podval.tools.publish.js

import zio.blocks.html.{Js, js}

final class Highlights(languages: Set[String]) extends JSLibrary:
  val version = "11.11.1"

  override def cdn: String = s"${JSLibrary.jsDelivr}/highlights@$version/lib"

  override val stylesheet: Some[String] = Some("/styles/default.min.css")

  override def imports: List[String] =
    List("/highlight.min.js") ++
    languages.map(language => s"/languages/$language.min.js")

  override val inlineJs: Some[Js] = Some(js"hljs.highlightAll();")
