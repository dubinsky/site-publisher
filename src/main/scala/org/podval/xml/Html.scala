package org.podval.xml

import zio.blocks.chunk.Chunk
import zio.blocks.html.Dom as XML

// XML AST for ZIO Blocks HTML
given Html: XmlAst[XML.Element]:
  type Node = XML
  
  override def text(text: String): Node = XML.text(text)

  override def element(elem: XmlElement): Element = XML.Element.Generic(
    tag = elem.name,
    children = Chunk.empty,
    attributes = Chunk.empty
  )

  extension (node: Node)
    override def asElement: Option[Element] = node match
      case element: XML.Element => Some(element)
      case _ => None

    override def asText: Option[String] = node.asAtom

    override def asAtom: Option[String] = node match
      case XML.Text(content) => Some(content)
      case _ => None

  extension (element: Element)
    override def getName: String = element.tag

    override def rename(name: String): Element = XML.Element.Generic(
      tag = name,
      attributes = element.attributes,
      children = element.children
    )

    override def getAttributes: Chunk[(String, String)] =
      element.attributes.map {
        case XML.Attribute.KeyValue(name, value) => (name, attributeValue(value))
        case XML.Attribute.BooleanAttribute(name, enabled) => (name, enabled.toString)
        case XML.Attribute.AppendValue(name, value, separator) => (name, attributeValue(value))
      }

    override def setAttributes(attributes: Chunk[(String, String)]): Element = XML.Element.Generic(
      tag = element.tag,
      children = element.children,
      attributes = attributes.map((name, value) => mkAttribute(name, value))
    )

    override def getChildren: Nodes = element.children

    override def setChildren(children: Nodes): Element = XML.Element.Generic(
      tag = element.tag,
      attributes = element.attributes,
      children = children
    )

  private def attributeValue(value: XML.AttributeValue): String = value match
    case XML.AttributeValue.StringValue(value) => value
    case XML.AttributeValue.BooleanValue(value) => value.toString
    case XML.AttributeValue.MultiValue(values, separator) => values.mkString(separator.render)
    case XML.AttributeValue.JsValue(value) => value.value

  private def attributeName(attribute: XML.Attribute): String = attribute match
    case XML.Attribute.KeyValue(name, _) => name
    case XML.Attribute.BooleanAttribute(name, _) => name
    case XML.Attribute.AppendValue(name, _, _) => name

  def mkAttribute(name: String, value: String) = XML.Attribute.KeyValue(
    name,
    XML.AttributeValue.StringValue(value)
  )
