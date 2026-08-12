package org.podval.tools.publish.js

import zio.blocks.html.{Js, js}

final class Highlights(languages: Set[String]) extends JSLibrary:
  val version: String: String = "11.11.1"

  override val stylesheet: Some[String] = Some("/styles/default.min.css")

  override def imports: List[String] =
    List("highlight.min.js") ++
    languages.map(language => s"languages/$language.min.js")

  override val inlineJs: Some[Js] = Some(js"hljs.highlightAll();")

  override def cdn: String =
    if JSLibrary.preferCloudFlare
    then s"${JSLibrary.cloudFlare}highlight.js/$version"
    else s"${JSLibrary.jsDelivr}@highlightjs/cdn-assets@$version"
