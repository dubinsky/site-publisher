package org.podval.xml

import org.podval.xml.codec.{XmlCodec, XmlError}
import zio.blocks.schema.Schema

/** AST-independent XML tree. Identity fields copy through any `XmlAst`. */
enum XmlNode derives CanEqual:
  case Element(name: String, attributes: Seq[(String, String)], children: Seq[XmlNode])
  case Text(value: String)
  case CData(value: String)

object XmlNode:
  given schema: Schema[XmlNode] = Schema.derived
  given elementSchema: Schema[Element] = Schema.derived

  def fromElement[E: XmlAst](element: E): Element =
    Element(
      name = element.getName,
      attributes = element.getAttributes,
      children = element.getChildren.flatMap(fromNode)
    )

  def fromNode[E](using ast: XmlAst[E])(node: ast.Node): Option[XmlNode] =
    node.asElement.map(fromElement)
      .orElse(node.asCData.map(CData.apply))
      .orElse(node.asText.map(Text.apply))

  def toElement[E: XmlAst](element: Element): E =
    summon[XmlAst[E]].element(
      element.name,
      element.attributes,
      element.children.map(toNode)
    )

  def toNode[E](using ast: XmlAst[E])(node: XmlNode): ast.Node = node match
    case element: Element => toElement(element)
    case Text(value) => ast.text(value)
    case CData(value) => ast.cdata(value)

  val elementCodec: XmlCodec[Element] = new XmlCodec[Element]:
    override def elementName: String = "element"
    override def isRecordLike: Boolean = true
    override def unsafeDecode[E: XmlAst](element: E): Element = fromElement(element)
    override def encodeNamed[E: XmlAst](name: String, value: Element): E =
      toElement(value.copy(name = name))
    override def encode[E: XmlAst](value: Element): E = toElement(value)

  val codec: XmlCodec[XmlNode] = new XmlCodec[XmlNode]:
    override def elementName: String = "node"
    override def isRecordLike: Boolean = true
    override def unsafeDecode[E: XmlAst](element: E): XmlNode = fromElement(element)
    override def encodeNamed[E: XmlAst](name: String, value: XmlNode): E = value match
      case element: Element => XmlNode.elementCodec.encodeNamed(name, element)
      case Text(text) => summon[XmlAst[E]].element(name, Seq.empty, Seq(summon[XmlAst[E]].text(text)))
      case CData(text) => summon[XmlAst[E]].element(name, Seq.empty, Seq(summon[XmlAst[E]].cdata(text)))
    override def encode[E: XmlAst](value: XmlNode): E = value match
      case element: Element => toElement(element)
      case _ => throw XmlError("Text and CData nodes encode only as children, not as a root element")
