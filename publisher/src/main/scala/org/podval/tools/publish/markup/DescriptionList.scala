package org.podval.tools.publish.markup

import org.podval.xml.{CssClass, Xml, XmlElement}
import Xml.given

object DescriptionList:
  def groupItems(
    nodes: Xml.Nodes,
    itemClass: CssClass,
    takeTermId: Xml.Element => (Option[String], Xml.Element)
  ): Xml.Nodes =
    var result: Xml.Nodes = Nil
    var group: Xml.Nodes = Nil
    var groupId: Option[String] = None

    def flush(): Unit =
      if group.nonEmpty then
        result = result :+ Xml
          .element(XmlElement.Div)
          .add(itemClass)
          .setId(groupId)
          .setChildren(group)
        group = Nil
        groupId = None

    nodes.foreach: node =>
      node.asElement match
        case Some(element) if element.isElement(XmlElement.Dt) =>
          flush()
          val (id, dt) = takeTermId(element)
          groupId = id
          group = Seq(dt)
        case Some(element) if element.isElement(XmlElement.Dd) =>
          if group.isEmpty then result = result :+ element
          else group = group :+ element
        case Some(element) =>
          flush()
          result = result :+ element
        case None =>
          if !node.isWhitespace then
            flush()
            result = result :+ node

    flush()
    result
