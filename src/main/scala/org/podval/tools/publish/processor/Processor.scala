package org.podval.tools.publish.processor

trait Processor:
  // Processor that needs to run at the end of its phase overrides this to `true`:
  // link/footnote processor needs to run after everything that was to be converted to links/footnotes had.
  def runLast: Boolean = false
