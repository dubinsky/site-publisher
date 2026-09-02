package org.podval.tools.publish.markup

import org.podval.xml.{HtmlClass, Xml}

object DescriptionList:
  def groupItems(
    nodes: Xml.Nodes,
    itemClass: HtmlClass,
    takeTermId: Xml.Element => (Option[String], Xml.Element)
  ): Xml.Nodes =
    var result: Xml.Nodes = Nil
    var group: Xml.Nodes = Nil
    var groupId: Option[String] = None

    def flush(): Unit =
      if group.nonEmpty then
        result = result :+ Xml
          .element("div")
          .add(itemClass)
          .setId(groupId)
          .setChildren(group)
        group = Nil
        groupId = None

    nodes.foreach: node =>
      node.asElement match
        case Some(element) if element.getName == "dt" =>
          flush()
          val (id, dt) = takeTermId(element)
          groupId = id
          group = Seq(dt)
        case Some(element) if element.getName == "dd" =>
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
