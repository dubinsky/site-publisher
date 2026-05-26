package org.podval.tools.publish.js

import zio.blocks.html.*

object Highlights:
  val version = "11.11.1"
  val cdn: String = s"${JSLibrary.jsDelivr}/highlights@$version/lib"

final class Highlights(languages: Set[String]) extends JSLibrary:
  import Highlights.cdn

  override val stylesheet: Some[String] = Some(s"$cdn/styles/default.min.css")

  override val scripts: List[Dom.Element.Script] =
    List(script().externalJs(s"$cdn/highlight.min.js")) ++
    languages.map(language => script().externalJs(languageModule(language))) ++
    List(script().inlineJs(js"hljs.highlightAll();"))

  private def languageModule(language: String): String =
    // NOT SUPPORTED   if language.toLowerCase == "liquid" then "https://unpkg.com/highlightjs-liquid@0.9.1/dist/liquid.min.js" else
    s"$cdn/languages/$language.min.js"
