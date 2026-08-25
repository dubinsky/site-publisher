package org.podval.tools.publish.site

trait PageErrorReporter:
  def error(
    kind: PageError.Kind,
    message: String,
    cause: Option[Throwable] = None
  ): Unit

object PageErrorReporter:
  object Silent extends PageErrorReporter:
    override def error(
      kind: PageError.Kind,
      message: String,
      cause: Option[Throwable] = None
    ): Unit = ()
  