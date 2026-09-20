package org.podval.tools.publish.js

import org.podval.xml.Xml
import org.podval.xml.dsl.{*, given}

abstract class JSLibrary:
  def cdn: String

  final protected def cdn(forCloudFlare: => String, forJsDelivr: => String): String =
    if JSLibrary.preferCloudFlare
    then forCloudFlare
    else forJsDelivr

  def stylesheet: Option[String] = None

  def imports: List[String] = List.empty

  def inlineJs: Option[Js] = None

  /** Inline script in `<head>` (e.g. apply a stored preference before paint). */
  def headInlineJs: Option[Js] = None

  def isModule: Boolean = false

  def inlineBeforeImports: Boolean = false

  /** Script tags for this library (imports + inline, order from [[inlineBeforeImports]]). */
  final def scripts: List[Xml.Element] =
    val imports: List[Xml.Element] = this.imports.map: path =>
      script().externalJs(s"$cdn/$path")

    val inlineJs: List[Xml.Element] = this.inlineJs.toList.map: code =>
      if isModule
      then script(typeAttr := "module").inlineJs(code.value)
      else script().inlineJs(code.value)

    if inlineBeforeImports
    then inlineJs ++ imports
    else imports ++ inlineJs

  /** Head inline script tags (`headInlineJs`); classic scripts, not `type=module`. */
  final def headScripts: List[Xml.Element] =
    headInlineJs.toList.map: code =>
      script().inlineJs(code.value)

object JSLibrary:
  val preferCloudFlare: Boolean = true

  // search: https://cdnjs.com/
  val cloudFlare: String = "https://cdnjs.cloudflare.com/ajax/libs/"

  val jsDelivr: String = "https://cdn.jsdelivr.net/npm/"
