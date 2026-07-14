package org.podval.tools.publish.js

object FontAwesome extends JSLibrary:
  val version: String = "7" //"7.0.0"
  
  override def cdn: String = s"${JSLibrary.jsDelivr}@fortawesome/fontawesome-free@$version"
  
  override val stylesheet: Some[String] = Some("/css/all.min.css")
