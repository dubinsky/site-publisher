package org.podval.tools.publish.js

object FontAwesome extends JSLibrary:
  val version: String = "7.3.0"

  override val stylesheet: Some[String] = Some("/css/all.min.css")
  
  override def cdn: String = cdn(
    s"${JSLibrary.cloudFlare}font-awesome/$version",
    s"${JSLibrary.jsDelivr}@fortawesome/fontawesome-free@$version"
  )
