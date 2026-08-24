package org.podval.tools.publish.markup

import org.podval.xml.{HtmlClass, Xml, XmlDialect}

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
    xml: Xml.Element,
    xmlDialect: XmlDialect
  ): Map[String, Xml.Nodes] =
    xmlDialect.gather(xml, element =>
      for
        id <- element.getId
        if isItem(element)
        nodes <- definitionNodes(element)
      yield id -> nodes
    ).toMap

  private def definitionNodes(item: Xml.Element): Option[Xml.Nodes] =
    item
      .getChildren
      .flatMap(_.asElement)
      .find(_.getName == "dd")
      .map(_.getChildren.filterNot(_.isWhitespace))
      .filter(_.nonEmpty)
