package org.podval.tools.publish.js

import zio.blocks.html.Js

abstract class JSLibrary:
  def cdn: String
  
  def stylesheet: Option[String] = None
  
  def imports: List[String] = List.empty
  
  def inlineJs: Option[Js] = None
  
  def isModule: Boolean = false

object JSLibrary:
  val preferCloudFlare: Boolean = true
  
  // search: https://cdnjs.com/
  val cloudFlare: String = "https://cdnjs.cloudflare.com/ajax/libs/"

  val jsDelivr: String = "https://cdn.jsdelivr.net/npm/"
