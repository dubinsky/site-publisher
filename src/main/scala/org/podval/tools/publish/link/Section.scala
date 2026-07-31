package org.podval.tools.publish.link

import org.podval.xml.HtmlClass

object Section:
  object SectionClass extends HtmlClass("section")

final class Section(
  val id: String,
  val title: String,
  override val sections: Seq[Section]
) extends Sections
