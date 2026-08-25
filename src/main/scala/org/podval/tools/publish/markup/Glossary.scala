package org.podval.tools.publish.markup

import org.podval.xml.{HtmlClass, Xml}

// Details of the glossary internal representation.
// Markup processors convert their HTML into this shape; definitions() does not
// know about AsciiDoc `div.dlist.glossary` wrappers or empty `<a id>` term anchors.
object Glossary:
  object ListClass extends HtmlClass("glossary")

  object ItemClass extends HtmlClass("glossary-item")

  val tip: Tip = Tip("glossary")

  def isList(element: Xml.Element): Boolean = element.has(ListClass)

  def isItem(element: Xml.Element): Boolean = element.has(ItemClass)

  def item(id: Option[String], children: Xml.Nodes): Xml.Element = Xml
    .element("div")
    .add(ItemClass)
    .setId(id)
    .setChildren(children)

  def definitions(
    xml: Xml.Element
  ): Map[String, Xml.Nodes] =
    xml.gather(element =>
      for
        id <- element.getId
        if isItem(element)
        nodes <- definitionNodes(element)
      yield id -> nodes
    ).toMap

  // Direct `dd`, or TagSoup's wrapper `<dl>` around `dt`/`dd` inside a `div`.
  private def definitionNodes(item: Xml.Element): Option[Xml.Nodes] =
    def dds(element: Xml.Element): Seq[Xml.Element] =
      element.getChildren.flatMap(_.asElement).toSeq.flatMap: child =>
        if child.getName == "dd" then Seq(child) else dds(child)
    dds(item).headOption
      .map(_.getChildren.filterNot(_.isWhitespace))
      .filter(_.nonEmpty)
