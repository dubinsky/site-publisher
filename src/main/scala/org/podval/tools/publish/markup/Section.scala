package org.podval.tools.publish.markup

import org.podval.tools.publish.util.IdGenerator
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

  lazy val path: Seq[Section] = parent.fold(Seq(this))(p => p.path :+ this)

object Section:
  private object SectionClass extends HtmlClass("section")
  object HeadingClass extends HtmlClass("heading")
  object AnchorClass extends HtmlClass("anchor")
  object LinkClass extends HtmlClass("link")

  def mark(element: Xml.Element): Xml.Element =
    require(element.getName == "div")
    element.add(SectionClass)

  def is(element: Xml.Element): Boolean =
    element.getName == "div" && element.has(SectionClass)

  def isPermalink(element: Xml.Element): Boolean =
    element.has(AnchorClass) || element.has(LinkClass)

  // Copy xml:id, ensure a section id, attach permalinks. Markup-independent IR.
  def normalize(element: Xml.Element, markup: Markup, ids: IdGenerator): Xml.Element =
    var result: Xml.Element = element.copyXmlId
    if is(result) then
      if result.getId.isEmpty then
        val title: Option[String] = markup.sectionHeader(result).map(headingText)
        result = result.setId(title.map(Xml.toId).getOrElse(ids.generate()))
      result = addPermalink(result, markup)
    result

  def headingText(header: Xml.Element): String =
    Xml.toString(header.getChildren.filterNot: node =>
      node.isWhitespace || node.asElement.exists(_.has(AnchorClass))
    ).trim

  def addPermalink(element: Xml.Element, markup: Markup): Xml.Element =
    if !is(element) then element else
      val id: Option[String] = element.getId.filter(_.nonEmpty)
      val header: Option[Xml.Element] = markup.sectionHeader(element)
      (id, header) match
        case (Some(id), Some(header)) if !header.has(HeadingClass) =>
          element.setChildren(element.getChildren.map: node =>
            if node.asElement.contains(header) then addLinks(header, id) else node
          )
        case _ =>
          element

  // AsciiDoctor sectanchors + sectlinks: hover §, heading text is a self-link.
  // If the heading already contains an <a> (e.g. a glossary term), only add the hover anchor.
  def addLinks(header: Xml.Element, id: String): Xml.Element =
    val href: String = s"#$id"
    val anchor: Xml.Element = Xml
      .element("a")
      .add(AnchorClass)
      .setHref(href)
      .set("aria-hidden", "true")
    val children: Xml.Nodes =
      if containsAnchor(header) then Chunk(anchor) ++ header.getChildren
      else
        val link: Xml.Element = Xml
          .element("a")
          .add(LinkClass)
          .setHref(href)
          .setChildren(header.getChildren)
        Chunk(anchor, link)
    header.add(HeadingClass).setChildren(children)

  private def containsAnchor(element: Xml.Element): Boolean =
    element.getChildren.exists: node =>
      node.asElement.exists(child => child.isA || containsAnchor(child))


