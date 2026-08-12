package org.podval.tools.publish.js

import zio.blocks.html.{Js, script, `type`}
import zio.blocks.html.Dom.Element.Script

abstract class JSLibrary:
  def cdn: String

  def stylesheet: Option[String] = None

  def imports: List[String] = List.empty

  def inlineJs: Option[Js] = None

  def isModule: Boolean = false

  def inlineBeforeImports: Boolean = false

  /** Script tags for this library (imports + inline, order from [[inlineBeforeImports]]). */
  final def scripts: List[Script] =
    val imports: List[Script] = this.imports.map: path =>
      script().externalJs(s"$cdn/$path")

    val inlineJs: List[Script] = this.inlineJs.toList.map: code =>
      // Note: `script` does *not* accept optional attributes;
      // Grok's fix making `Element.when Self-typed is in my ZIO Blocks repository. 
      if isModule
      then script(`type` := "module").inlineJs(code)
      else script().inlineJs(code)

    if inlineBeforeImports
    then inlineJs ++ imports
    else imports ++ inlineJs

object JSLibrary:
  val preferCloudFlare: Boolean = true

  // search: https://cdnjs.com/
  val cloudFlare: String = "https://cdnjs.cloudflare.com/ajax/libs/"

  val jsDelivr: String = "https://cdn.jsdelivr.net/npm/"
