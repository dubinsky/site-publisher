package org.podval.tools.publish.markup

import org.podval.xml.{HtmlClass, Xml}
import org.podval.tools.publish.util.IdGenerator

final class Section(
  val id: String,
  val title: String,
  override val sections: Seq[Section]
) extends Sections

object Section:
  object SectionClass extends HtmlClass("section")
  
  def is(element: Xml.Element): Boolean =
    element.getName == "div" && element.has(SectionClass)

  def setSectionId(element: Xml.Element, ids: IdGenerator): Option[Xml.Element] =
    Option.when(Section.is(element) && element.getId.isEmpty)(
      element.setId(ids.generate())
    )
