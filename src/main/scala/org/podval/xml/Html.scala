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
     * ZIO Blocks may store multi-valued attrs (e.g. `className += …`) as
     * [[XML.Attribute.AppendValue]] entries.
     * Dom.render, which is not public,  merges those before
     * emission; we must do the same here or XmlWriter emits duplicate attribute
     * names and loses values. Boolean attributes pass through.
     */
    // Note: written by Grok ;)
    override def getAttributes: Chunk[(String, String)] =
      val attrs: Chunk[XML.Attribute] = element.attributes

      import XML.Attribute.*

      val hasAppend: Boolean = attrs.exists:
        case _: AppendValue => true
        case _              => false

      val hasDuplicateKeyValues: Boolean =
        val seen = scala.collection.mutable.HashSet.empty[String]
        attrs.exists:
          case KeyValue(name, _) => !seen.add(name)
          case _                 => false

      if !hasAppend && !hasDuplicateKeyValues then
        attrs.map:
          case KeyValue(name, value)              => (name, attributeValue(value))
          case BooleanAttribute(name, enabled)    => (name, enabled.toString)
          case AppendValue(name, value, _)        => (name, attributeValue(value))
      else
        val keyValues = java.util.LinkedHashMap[String, XML.AttributeValue]()
        val appends   = java.util.LinkedHashMap[String, java.util.ArrayList[(XML.AttributeValue, XML.AttributeSeparator)]]()

        var i = 0
        while i < attrs.length do
          attrs(i) match
            case KeyValue(name, value) =>
              keyValues.put(name, value)
            case AppendValue(name, value, sep) =>
              var list = appends.get(name)
              if list == null then
                list = java.util.ArrayList()
                appends.put(name, list)
              list.add((value, sep))
            case _: BooleanAttribute => ()
          i += 1

        val merged = java.util.HashMap[String, String]()
        val appendIter = appends.entrySet().iterator()
        while appendIter.hasNext do
          val entry = appendIter.next()
          val name  = entry.getKey
          val list  = entry.getValue
          val sb    = StringBuilder()
          Option(keyValues.get(name)).foreach(appendValue(_, sb))
          var k = 0
          while k < list.size do
            val (av, sep) = list.get(k)
            if sb.nonEmpty then sb.append(sep.render)
            appendValue(av, sb)
            k += 1
          merged.put(name, sb.result())

        val lastKeyValueIndex = java.util.HashMap[String, Int]()
        var m = 0
        while m < attrs.length do
          attrs(m) match
            case KeyValue(name, _) => lastKeyValueIndex.put(name, m)
            case _                 => ()
          m += 1

        val result  = Chunk.newBuilder[(String, String)]
        val emitted = java.util.HashSet[String]()
        var n = 0
        while n < attrs.length do
          attrs(n) match
            case BooleanAttribute(name, enabled) =>
              result += ((name, enabled.toString))

            case KeyValue(name, _) if merged.containsKey(name) =>
              // Name has appends: emit merged value at the last KeyValue position.
              if !emitted.contains(name) && lastKeyValueIndex.get(name) == n then
                result += ((name, merged.get(name)))
                emitted.add(name)

            case KeyValue(name, value) =>
              // No appends: last KeyValue wins.
              if lastKeyValueIndex.get(name) == n then
                result += ((name, attributeValue(value)))

            case AppendValue(name, _, _) =>
              // Append-only name: emit merged value at the first AppendValue.
              if !emitted.contains(name) && !keyValues.containsKey(name) then
                result += ((name, merged.get(name)))
                emitted.add(name)

          n += 1

        result.result()

    private def appendValue(value: XML.AttributeValue, sb: StringBuilder): Unit =
      value match
        case XML.AttributeValue.StringValue(v) =>
          sb.append(v)
        case XML.AttributeValue.MultiValue(values, sep) =>
          var i = 0
          while i < values.length do
            if i > 0 then sb.append(sep.render)
            sb.append(values(i))
            i += 1
        case XML.AttributeValue.BooleanValue(v) =>
          sb.append(v.toString)
        case XML.AttributeValue.JsValue(js) =>
          sb.append(js.value)

    private def attributeValue(value: XML.AttributeValue): String = value match
      case XML.AttributeValue.StringValue(value) => value
      case XML.AttributeValue.BooleanValue(value) => value.toString
      case XML.AttributeValue.MultiValue(values, separator) => values.mkString(separator.render)
      case XML.AttributeValue.JsValue(value) => value.value

    private def mkAttribute(name: String, value: String) = XML.Attribute.KeyValue(
      name,
      XML.AttributeValue.StringValue(value)
    )
