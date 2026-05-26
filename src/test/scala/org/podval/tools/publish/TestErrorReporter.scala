package org.podval.tools.publish

final class TestErrorReporter extends PageError.Reporter:
  var kind: Option[PageError.Kind] = None
  var message: Option[String] = None
  var cause: Option[Throwable] = None
  
  def empty: Boolean = kind.isEmpty
  
  override def error[R](
    kind: PageError.Kind,
    message: String,
    result: R,
    cause: Option[Throwable] = None
  ): R =
    this.kind = Some(kind)
    this.message = Some(message)
    this.cause = cause
    result
    