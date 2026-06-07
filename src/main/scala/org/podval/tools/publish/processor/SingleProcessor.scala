package org.podval.tools.publish.processor

abstract class SingleProcessor extends Processor:
  final override def processors: Seq[SingleProcessor] = Seq(this)
  
  