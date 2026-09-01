package org.podval.xml

import scala.annotation.tailrec

// XML AST for ScalaXml
given ScalaXml: XmlAst[scala.xml.Elem]:
  override type Node = scala.xml.Node
  
  override def text(text: String): Node = scala.xml.Text(text)

  override def cdata(text: String): Node = scala.xml.PCData(text) // TODO CData/PCData? Unparsed?

  override def element(name: String): Element = scala.xml.Elem(
    prefix = null,
    label = null,
    attributes = scala.xml.Null,
    scope = scala.xml.TopScope,
    minimizeEmpty = false
  )

  extension (node: Node)
    override def asElement: Option[Element] = node match
      case element: scala.xml.Elem => Some(element)
      case _ => None

    override def asText: Option[String] = node match
      case text: scala.xml.Text => Some(text.data)
      case _ => None

    override def asCData: Option[String] = node match
      case cdata: scala.xml.PCData => Some(cdata.data)
      case _ => None

    override def asAtom: Option[String] = node match
      case text: scala.xml.Text => Some(text.data)
      case cdata: scala.xml.PCData => Some(cdata.data)
      case _ => None

  extension (element: Element)
    override def getName: String =
      element.label

    override def rename(name: String): Element =
      element.copy(label = name)
    
    override def getChildren: Nodes =
      element.child

    override def setChildren(children: Nodes): Element =
      element.copy(child = children)

    override def getAttributes: Seq[(String, String)] =
      element.attributes.asAttrMap.toSeq
    
    override def setAttributes(attributes: Seq[(String, String)]): Element =
      @tailrec
      def loop(attributes: Seq[(String, String)], result: scala.xml.MetaData): scala.xml.MetaData =
        if attributes.isEmpty
        then result
        else
          val (key, value) = attributes.last
          loop(attributes.init, scala.xml.UnprefixedAttribute(key, value, result))
      element.copy(attributes = loop(attributes, scala.xml.Null)) 
