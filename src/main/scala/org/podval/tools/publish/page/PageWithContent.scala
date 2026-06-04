package org.podval.tools.publish.page

import org.podval.tools.publish.util.Files

trait PageWithContent extends Page:
  final override def write(): Unit = Files.write(targetFile, content)

  def content: String
