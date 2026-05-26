package org.podval.tools.publish.js

object FontAwesome extends JSLibrary:
  val version: String = "7.0.0"
  private val cdn: String = s"${JSLibrary.jsDelivr}/@fortawesome/fontawesome-free@$version"
  override val stylesheet: Some[String] = Some(s"$cdn/css/all.min.css")
