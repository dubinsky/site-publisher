package org.podval.tools.publish.processor

abstract class Processor:
  def processors: Seq[SingleProcessor]
  
