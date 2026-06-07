package org.podval.tools.publish.processor

open class Processors(val subProcessors: Processor*) extends Processor:
  final override def processors: Seq[SingleProcessor] = subProcessors.flatMap(_.processors)
