package org.podval.xml

import zio.blocks.chunk.Chunk
import zio.blocks.html.Dom as XML

// XML AST for ZIO Blocks HTML
given Html: XmlAst[XML.Element]:
  type Node = XML
  
  override def text(text: String): Node = XML.text(text)

  override def element(name: String): Element = XML.Element.Generic(
    tag = name,
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

    override def getChildren: Nodes = element.children

    override def setChildren(children: Nodes): Element = XML.Element.Generic(
      tag = element.tag,
      attributes = element.attributes,
      children = children
    )

    override def setAttributes(attributes: Chunk[(String, String)]): Element = XML.Element.Generic(
      tag = element.tag,
      children = element.children,
      attributes = attributes.map((name, value) => mkAttribute(name, value))
    )

    /**
     * Merge ZIO Blocks multi-valued attrs (`className += …` is an AppendValue)
     * as Dom.render does: last `:=` is the base, then every `+=` in order.
     * One pair per name (first-seen order). Boolean attributes pass through.
     */
    override def getAttributes: Chunk[(String, String)] =
      enum Merged:
        case Flag(enabled: Boolean)
        case Text(base: Option[String], extras: Vector[(String, String)])

      val byName = scala.collection.mutable.LinkedHashMap.empty[String, Merged]

      element.attributes.foreach:
        case XML.Attribute.BooleanAttribute(name, enabled) =>
          byName(name) = Merged.Flag(enabled)

        case XML.Attribute.KeyValue(name, value) =>
          byName(name) = Merged.Text(
            base = Some(attributeValue(value)),
            extras = byName.get(name) match
              case Some(Merged.Text(_, extras)) => extras
              case _                            => Vector.empty
          )

        case XML.Attribute.AppendValue(name, value, sep) =>
          val extra = (sep.render, attributeValue(value))
          byName(name) = byName.get(name) match
            case Some(Merged.Text(base, extras)) =>
              Merged.Text(base, extras :+ extra)
            case _ =>
              Merged.Text(None, Vector(extra))

      Chunk.from:
        byName.iterator.map:
          case (name, Merged.Flag(enabled)) =>
            (name, enabled.toString)
          case (name, Merged.Text(base, extras)) =>
            (name, (base ++ extras.map(_._2)).mkString(extras.headOption.fold("")(_._1)))

    private def attributeValue(value: XML.AttributeValue): String = value match
      case XML.AttributeValue.StringValue(value) => value
      case XML.AttributeValue.BooleanValue(value) => value.toString
      case XML.AttributeValue.MultiValue(values, separator) => values.mkString(separator.render)
      case XML.AttributeValue.JsValue(value) => value.value

    private def mkAttribute(name: String, value: String) = XML.Attribute.KeyValue(
      name,
      XML.AttributeValue.StringValue(value)
    )
