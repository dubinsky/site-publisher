package org.podval.xml

object XmlUtil:
  def toId(text: String): String = text.trim.replace(' ', '-')

  // ZIO Blocks `Chunk.flatMap` / `++` (Scala `appendedAll`) take ClassTag from the first
  // inner chunk, so a leading text node then an element (or the reverse) throws
  // ArrayStoreException. `:+` uses an AnyRef buffer, as the SAX builder does.
  def flatMapNodes(nodes: Xml.Nodes, f: Xml.Node => Xml.Nodes): Xml.Nodes =
    nodes.foldLeft(Seq.empty[Xml.Node]): (acc, node) =>
      f(node).foldLeft(acc)(_ :+ _)

  def convertText(
    element: Xml.Element,
    converter: String => Xml.Nodes
  ): Xml.Element =
    element.setChildren(flatMapNodes(element.getChildren, xml =>
      xml.asText.fold(Seq(xml))(converter)
    ))

  def convertElements(
    children: Xml.Nodes,
    converter: Xml.Element => Option[Xml.Nodes]
  ): Xml.Nodes =
    flatMapNodes(children, child =>
      child.asElement.flatMap(converter).getOrElse(Seq(child))
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

  def isInclude(element: Xml.Element): Boolean =
    element.localName == "include" && element.get("href").exists(_.trim.nonEmpty)

  // Note: I do not see any reason to recognize elements (like 'script') or attributes (like 'hidden')...
  def xml2html(element: Xml.Element): Html.Element = Html
    .element(element.getName)
    .setAttributes(element.getAttributes)
    .setChildren(
      element.getChildren.foldLeft(Seq.empty[Html.Node]): (acc, child) =>
        // ZIO Blocks HTML does not support comments nor processing instructions
        child.asElement.map(xml2html)
          .orElse(child.asAtom.map(Html.text))
          .fold(acc)(acc :+ _)
    )
