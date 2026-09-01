package org.podval.xml

import zio.blocks.chunk.Chunk
import zio.blocks.schema.xml.{XmlBuilder, XmlName, Xml as XML}

// XML AST for ZIO Blocks XML
given Xml: XmlAst[XML.Element]:
  override type Node = XML
  
  override def text(text: String): Node = XML.Text(text)

  override def cdata(text: String): Node = XML.CData(text)

  override def element(name: String): Element = XmlBuilder.element(name).build

  extension (node: Node)
    override def asElement: Option[Element] = node match
      case element: XML.Element => Some(element)
      case _ => None

    override def asText: Option[String] = node match
      case XML.Text(value) => Some(value)
      case _ => None

    override def asCData: Option[String] = node match
      case XML.CData(value) => Some(value)
      case _ => None

    override def asAtom: Option[String] = node match
      case XML.Text(value) => Some(value)
      case XML.CData(value) => Some(value)
      case _ => None

  extension (element: Element)
    override def getName: String =
      element.name.qualifiedName

    override def rename(name: String): Element =
      element.copy(name = XmlName(name))

    override def getAttributes: Seq[(String, String)] =
      element.attributes.map((xmlName, value) => (xmlName.qualifiedName, value))

    override def setAttributes(attributes: Seq[(String, String)]): Element =
      element.copy(attributes = Chunk.from(attributes).map((name, value) => (XmlName(name), value)))

    override def getChildren: Nodes =
      element.children

    override def setChildren(children: Nodes): Element =
      element.copy(children = Chunk.from(children))
