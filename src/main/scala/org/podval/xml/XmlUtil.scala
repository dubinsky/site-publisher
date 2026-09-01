package org.podval.xml

import zio.blocks.chunk.Chunk

object XmlUtil:
  def toId(text: String): String = text.trim.replace(' ', '-')

  def convertText(
    element: Xml.Element,
    converter: String => Xml.Nodes
  ): Xml.Element =
    element.setChildren(
      element.getChildren.foldLeft(Chunk.empty[Xml.Node]): (acc, xml) =>
        acc ++ xml.asText.fold(Chunk(xml))(converter)
    )

  def convertElements(
    children: Xml.Nodes,
    converter: Xml.Element => Option[Xml.Nodes]
  ): Xml.Nodes =
    // Do not use `Chunk.flatMap`: it takes ClassTag from the first inner chunk, so a
    // leading text node then an element (or the reverse) throws ArrayStoreException.
    children.foldLeft(Chunk.empty[Xml.Node]): (acc, child) =>
      acc ++ child.asElement.flatMap(converter).getOrElse(Chunk(child))

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

  def isInclude(element: Xml.Element): Boolean =
    element.localName == "include" && element.get("href").exists(_.trim.nonEmpty)

  // Note: I do not see any reason to recognize elements (like 'script') or attributes (like 'hidden')...
  def xml2html(element: Xml.Element): Html.Element = Html
    .element(element.getName)
    .setAttributes(element.getAttributes)
    .setChildren(element.getChildren.flatMap: child =>
      // ZIO Blocks HTML does not support comments nor processing instructions
      child.asElement.map(xml2html)
        .orElse(child.asAtom.map(Html.text))
    )
