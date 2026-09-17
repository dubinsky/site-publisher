package org.podval.tools.publish.site

import org.podval.metadata.Language

trait PageErrorReporter:
  def error(
    kind: PageError.Kind,
    message: String,
    cause: Option[Throwable] = None
  ): Unit

  def languageSpec: Language.Spec = Language.English.toSpec

  def teiDefaultCalendarIsJulian: Boolean = false

object PageErrorReporter:
  object Silent extends PageErrorReporter:
    override def error(
      kind: PageError.Kind,
      message: String,
      cause: Option[Throwable] = None
    ): Unit = ()
  