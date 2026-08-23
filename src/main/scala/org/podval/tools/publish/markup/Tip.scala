package org.podval.tools.publish.markup

import org.podval.xml.{HtmlClass, Xml}
import zio.blocks.chunk.Chunk

final class Tip(prefix: String):
  object RefClass extends HtmlClass(s"$prefix-ref")
  object TipClass extends HtmlClass(s"$prefix-tip")

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

  def isTip(node: Xml.Node): Boolean =
    node.asElement.exists(_.has(TipClass))
