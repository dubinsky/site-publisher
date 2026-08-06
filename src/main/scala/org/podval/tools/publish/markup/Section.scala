package org.podval.tools.publish.markup

import org.podval.xml.{HtmlClass, Xml}

final class Section(
  val id: String,
  val title: String,
  sections: Seq[Section]
) extends Sections(sections):
  override def toString: String = id
  
  sections.foreach(_.setParent(this))

  private var parentVar: Option[Section] = None
  def setParent(parent: Section): Unit = parentVar = Some(parent)
  def parent: Option[Section] = parentVar
  
  lazy val depth: Int = parentVar.fold(0)(_.depth + 1)

object Section:
  private object SectionClass extends HtmlClass("section")

  def mark(element: Xml.Element): Xml.Element =
    require(element.getName == "div")
    element.add(SectionClass)

  def is(element: Xml.Element): Boolean =
    element.getName == "div" && element.has(SectionClass)


