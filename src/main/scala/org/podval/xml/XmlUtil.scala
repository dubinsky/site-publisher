package org.podval.xml

import zio.blocks.chunk.Chunk

object XmlUtil:
  def convertText(
    element: Xml.Element,
    converter: String => Xml.Nodes
  ): Xml.Element =
    element.setChildren(element.getChildren.flatMap(xml => xml.asText.fold(Chunk(xml))(converter)))

  def convertElements(
    children: Xml.Nodes,
    converter: Xml.Element => Option[Xml.Nodes]
  ): Xml.Nodes = children.flatMap(child =>
    child.asElement.flatMap(converter).getOrElse(Chunk(child))
  )

  def renameElement(
    name: String,
    element: Xml.Element
  ): Xml.Element = element
    .addClass(element.getName)
    .rename(name)

  def copyAttribute(
    from: String,
    to: String,
    element: Xml.Element
  ): Xml.Element =
    element.get(from).fold(element)(element.set(to, _))

  def elementById(
    xml: Xml.Element,
    id: String
  ): Xml.Element = xml
    .gather(element => Option.when(element.getId.contains(id))(element))
    .head
