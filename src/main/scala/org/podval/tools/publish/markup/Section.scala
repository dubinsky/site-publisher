package org.podval.tools.publish.markup

import org.podval.xml.{HtmlClass, Xml}
import zio.blocks.chunk.Chunk

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
  private object AnchorClass extends HtmlClass("anchor")

  def mark(element: Xml.Element): Xml.Element =
    require(element.getName == "div")
    element.add(SectionClass)

  def is(element: Xml.Element): Boolean =
    element.getName == "div" && element.has(SectionClass)

  def isPermalink(element: Xml.Element): Boolean =
    element.has(AnchorClass)

  // AsciiDoctor-style sectanchors: empty hover permalink; heading text stays plain.
  def addAnchor(header: Xml.Element, id: String): Xml.Element =
    val anchor: Xml.Element = Xml
      .element("a")
      .add(AnchorClass)
      .setHref(s"#$id")
      .set("aria-hidden", "true")
    header.setChildren(Chunk(anchor) ++ header.getChildren)


