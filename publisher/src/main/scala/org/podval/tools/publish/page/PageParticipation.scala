package org.podval.tools.publish.page

final case class PageParticipation(
  listedInDirectory: Boolean = false,
  leafIndex: Boolean = false,
  leafAlias: Boolean = false,
  absorption: PageParticipation.Absorption = PageParticipation.Absorption.None
) derives CanEqual

object PageParticipation:
  /** `AnyMarkup` and `MarkupProse` are handled by `Pages.absorbAtPath`. */
  enum Absorption derives CanEqual:
    /** Scan-created, or a synthetic page that does not consume a file. */
    case None
    case AnyMarkup
    case MarkupProse

  val default: PageParticipation = PageParticipation()
