package org.podval.tools.publish.page

trait NonDirectoryPage extends Page:
  final override def isDirectory: Boolean = false

  final override def titleFromPath: String = path.fileName
