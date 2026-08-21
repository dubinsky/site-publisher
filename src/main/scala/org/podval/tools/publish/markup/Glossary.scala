package org.podval.tools.publish.markup

import org.podval.xml.{HtmlClass, Xml, XmlDialect, XmlUtil}
import zio.blocks.chunk.Chunk

// Note: written by Grok ;)
object Glossary:
  object ItemClass extends HtmlClass("dlist-item")

  object RefClass extends HtmlClass("glossary-ref")

  object TipClass extends HtmlClass("glossary-tip")

  def definitions(
    xml: Xml.Element,
    xmlDialect: XmlDialect
  ): Map[String, Xml.Nodes] =
    xmlDialect.gatherWithContext(
      xml,
      // TODO this looks like an AsciiDoc-specific class;
      // I should define - and convert to - a markup-independent internal representation for glossary...
      isContext = _.hasClass("glossary"),
      gatherElement = (element, glossary) =>
        for
          _ <- glossary
          id <- element.getId
          if element.has(ItemClass)
          dd <- element.getChildren.flatMap(_.asElement).find(_.getName == "dd")
          children = dd.getChildren.filterNot(isBlankText)
          if children.nonEmpty
        yield id -> children
    ).toMap

  def attachTip(link: Xml.Element, definition: Xml.Nodes): Xml.Element =
    var tip: Xml.Element = Xml
      .element("span")
      .add(TipClass)
      .setChildren(definition)
    var result: Xml.Element = link
    link.getId.foreach: id =>
      val tipId: String = s"$id-tip"
      tip = tip.setId(tipId).set("role", "tooltip")
      result = result.set("aria-describedby", tipId)
    result.setChildren(result.getChildren :+ tip)

  def wrapRef(element: Xml.Element): Option[Xml.Nodes] =
    if !element.isA then None else
      val children: Xml.Nodes = element.getChildren
      val tips: Xml.Nodes = children.filter(isTip)
      Option.when(tips.nonEmpty):
        val link: Xml.Element = element.setChildren(children.filterNot(isTip))
        Chunk(
          Xml
            .element("span")
            .add(RefClass)
            .setChildren(link +: tips)
        )

  def wrapRefs(element: Xml.Element): Xml.Element =
    element.setChildren(XmlUtil.convertElements(element.getChildren, wrapRef))

  private def isTip(node: Xml.Node): Boolean =
    node.asElement.exists(_.has(TipClass))

  private def isBlankText(node: Xml.Node): Boolean =
    node.asText.exists(_.trim.isEmpty)
