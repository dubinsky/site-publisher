package org.podval.tools.publish.markup

import org.podval.xml.{HtmlClass, Xml}
import zio.blocks.chunk.Chunk

/** Markup-neutral aside IR. CSS styles only these classes.
  * Untyped auxiliary content (`<aside class="aside">`), optional title. */
object Aside:
  object Class extends HtmlClass("aside")
  object TitleClass extends HtmlClass("aside-title")

  def is(element: Xml.Element): Boolean =
    element.getName == "aside" && element.has(Class)

  def isTitle(element: Xml.Element): Boolean = element.has(TitleClass)

  def make(title: Option[String], body: Xml.Nodes): Xml.Element =
    val titleElement: Option[Xml.Element] = title.map(_.trim).filter(_.nonEmpty).map: label =>
      Xml.element("div").add(TitleClass).setText(label)
    Xml
      .element("aside")
      .add(Class)
      .setChildren(Chunk.from(titleElement.toSeq) ++ body.filterNot(_.isWhitespace))

  def normalize(element: Xml.Element): Xml.Element =
    if element.getName != "aside" || is(element) then element
    else element.add(Class)
