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

  /** Interface `lang` and `dir` for `a.pb`, `span.pb`, and `span.date-tip` when the codes differ.
    * Silent has none, so those nodes stay unstamped. */
  def interfaceChrome: Option[(String, String)] = None

object PageErrorReporter:
  object Silent extends PageErrorReporter:
    override def error(
      kind: PageError.Kind,
      message: String,
      cause: Option[Throwable] = None
    ): Unit = ()
  