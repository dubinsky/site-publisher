package org.podval.xml

import zio.blocks.chunk.Chunk
import zio.blocks.html.Dom as XML

// XML AST for ZIO Blocks HTML
object Html extends XmlAst:
  override type Xml = XML
  override type Element = XML.Element

  val writer: XmlWriter[Html.type] = XmlWriter(Html)(XmlWriter.htmlWriterConfig)

  override def asElement(xml: Xml): Option[Element] = xml match
    case element: XML.Element => Some(element)
    case _ => None

  override def name(element: Element): String = element.tag

  override def rename(element: Element, name: String): Element = XML.Element.Generic(
    tag = name,
    attributes = element.attributes,
    children = element.children
  )

  override def element(name: String): Element = XML.Element.Generic(
    tag = name,
    children = Chunk.empty,
    attributes = Chunk.empty
  )

  override def attributes(element: Element, parent: Option[Element]): Chunk[(String, String)] =
    attributes(element)
    
  override def attributes(element: Element): Chunk[(String, String)] =
    element.attributes.map {
      case XML.Attribute.KeyValue(name, value) => (name, attributeValue(value))
      case XML.Attribute.BooleanAttribute(name, enabled) => (name, enabled.toString)
      case XML.Attribute.AppendValue(name, value, separator) => (name, attributeValue(value))
    }

  private def attributeValue(value: XML.AttributeValue): String = value match
    case XML.AttributeValue.StringValue(value) => value
    case XML.AttributeValue.BooleanValue(value) => value.toString
    case XML.AttributeValue.MultiValue(values, separator) => values.mkString(separator.render)
    case XML.AttributeValue.JsValue(value) => value.value

  private def attributeName(attribute: XML.Attribute): String = attribute match
    case XML.Attribute.KeyValue(name, _) => name
    case XML.Attribute.BooleanAttribute(name, _) => name
    case XML.Attribute.AppendValue(name, _, _) => name

  override def setAttributes(element: Element, attributes: Chunk[(String, String)]): Element = XML.Element.Generic(
    tag = element.tag,
    children = element.children,
    attributes = attributes.map((name, value) => mkAttribute(name, value))
  )

  override def setAttribute(element: Element, name: String, value: String): Element = XML.Element.Generic(
    tag = element.tag,
    children = element.children,
    attributes = element.attributes
      .filterNot(attribute => attributeName(attribute) == name)
      .appended(mkAttribute(name, value))
  )

  private def mkAttribute(name: String, value: String) = XML.Attribute.KeyValue(
    name,
    XML.AttributeValue.StringValue(value)
  )
  
  override def children(element: Element): Chunk[Xml] = element.children

  override def setChildren(element: Element, children: Chunk[Xml]): Element = XML.Element.Generic(
    tag = element.tag,
    attributes = element.attributes,
    children = children
  )
  
  override def mkText(text: String): Xml = XML.text(text)

  override def asText(xml: Xml): Option[String] = asAtom(xml)

  override def asAtom(xml: Xml): Option[String] = xml match
    case XML.Text(content) => Some(content)
    case _ => None

  import zio.blocks.schema.xml.Xml as From

  // Note: I do not see any reason to recognize elements (like 'script') or attributes (like 'hidden')...
  def fromXml(element: From.Element): XML.Element = XML.Element.Generic(
    tag = element.name.qualifiedName,
    attributes = element.attributes.map((name, value) => mkAttribute(name.qualifiedName, value)),
    children = element.children.flatMap {
      case From.Comment(value) => None
      case From.ProcessingInstruction(target, data) => None
      case From.Text(value) => Some(mkText(value))
      case From.CData(value) => Some(mkText(value))
      case element: From.Element => Some(fromXml(element))
    }
  )
