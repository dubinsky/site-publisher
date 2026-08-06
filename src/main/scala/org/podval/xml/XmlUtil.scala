package org.podval.xml

object XmlUtil:
  // TODO convertText() should move here too.

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
    id: String,
    xmlDialect: XmlDialect
  ): Xml.Element = xmlDialect
    .gather(xml, element => Option.when(element.getId.contains(id))(element))
    .head
