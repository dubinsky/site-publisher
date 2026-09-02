package org.podval.xml

// XML AST for Scala XML
given ScalaXml: XmlAst[scala.xml.Elem]:
  override type Node = scala.xml.Node

  override def text(text: String): Node = scala.xml.Text(text)

  override def cdata(text: String): Node = scala.xml.PCData(text)

  override def element(name: String): Element =
    val (prefix, label) = splitQualified(name)
    scala.xml.Elem(
      prefix = prefix,
      label = label,
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

    override def asAtom: Option[String] = node.asText.orElse(node.asCData)

  extension (element: Element)
    override def getName: String =
      qualifiedName(element.prefix, element.label)

    override def rename(name: String): Element =
      val (prefix, label) = splitQualified(name)
      element.copy(prefix = prefix, label = label)

    override def getChildren: Nodes =
      element.child

    override def setChildren(children: Nodes): Element =
      element.copy(child = children)

    override def getAttributes: Seq[(String, String)] =
      element.attributes.iterator.map: attribute =>
        (attribute.prefixedKey, scala.xml.NodeSeq.fromSeq(attribute.value).text)
      .toSeq

    override def setAttributes(attributes: Seq[(String, String)]): Element =
      val metadata: scala.xml.MetaData = attributes.foldRight(scala.xml.Null: scala.xml.MetaData):
        case ((key, value), next) =>
          val (prefix, local) = splitQualified(key)
          scala.xml.Attribute(prefix, local, value, next)
      element.copy(attributes = metadata)

  // scala.xml rejects prefix ""; unprefixed names use null.
  private def splitQualified(name: String): (String, String) =
    val colon: Int = name.indexOf(':')
    if colon <= 0 then (null, name)
    else (name.substring(0, colon), name.substring(colon + 1))

  private def qualifiedName(prefix: String, label: String): String =
    if prefix == null then label else s"$prefix:$label"
