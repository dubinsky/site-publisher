package org.podval.tools.publish.markup

import org.podval.xml.{HtmlClass, Xml, XmlAttribute}
import zio.blocks.chunk.Chunk

/** Markup-neutral admonition IR. CSS styles only these classes.
  * Type is `data-type` (lowercase). Optional Obsidian fold is a `<details>`. */
object Admonition:
  object Class extends HtmlClass("admonition")
  object TitleClass extends HtmlClass("admonition-title")
  object TypeAttr extends XmlAttribute("data-type")

  def is(element: Xml.Element): Boolean = element.has(Class)

  def isTitle(element: Xml.Element): Boolean = element.has(TitleClass)

  def make(
    typeName: String,
    title: Option[String],
    body: Xml.Nodes,
    fold: Option[Boolean] = None
  ): Xml.Element =
    val kind: String = typeName.trim.toLowerCase
    val label: String = title.map(_.trim).filter(_.nonEmpty).getOrElse(displayTitle(kind))
    val titleElement: Xml.Element = fold match
      case Some(_) => Xml.element("summary").add(TitleClass).setText(label)
      case None => Xml.element("div").add(TitleClass).setText(label)
    val children: Xml.Nodes = titleElement +: body.filterNot(_.isWhitespace)
    val element: Xml.Element = fold match
      case Some(true) => Xml.element("details").set("open", "open")
      case Some(false) => Xml.element("details")
      case None => Xml.element("div")
    element.add(Class).set(TypeAttr, kind).setChildren(children)

  def displayTitle(typeName: String): String =
    if typeName.isEmpty then "Note"
    else typeName.head.toUpper.toString + typeName.tail
