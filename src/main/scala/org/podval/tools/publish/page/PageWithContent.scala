package org.podval.tools.publish.page

import org.podval.tools.publish.util.Files

trait PageWithContent extends Page:
  override def write(): Unit = Files.write(targetFile, textContent)

  def textContent: String
