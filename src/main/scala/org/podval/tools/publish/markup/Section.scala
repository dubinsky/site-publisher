package org.podval.tools.publish.markup

import org.podval.xml.{HtmlClass, Xml}

object Section:
  object SectionClass extends HtmlClass("section")
  
  def is(element: Xml.Element): Boolean =
    element.getName == "div" && element.has(SectionClass)

final class Section(
  val id: String,
  val title: String,
  override val sections: Seq[Section]
) extends Sections
