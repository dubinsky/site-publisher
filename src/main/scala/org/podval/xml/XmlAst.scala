package org.podval.xml

// AST that represents XML and provides operations on it;
// abstracts over the underlying representation:
// - ZIO Blocks XML
// - ZIO Blocks HTML
// - potentially Scala XML
// - potentially DOM
trait XmlAst[ELEMENT]:
  final type Element = ELEMENT

  type Node >: Element

  // TODO replace Seq[Node] with Nodes throughout
  final type Nodes = Seq[Node]

  def text(text: String): Node

  def cdata(text: String): Node

  final def element(elem: XmlElement): Element = element(elem.name)

  def element(name: String): Element

  // Concatenate only: text nodes already carry author whitespace. Joining with a space
  // puts a gap before punctuation after inline markup (`</persName>,` → "е ,").
  final def toString(nodes: Nodes): String = nodes.map(_.getText).mkString

  // Conversions
  extension (node: Node)
    def asElement: Option[Element]
    
    def asAtom: Option[String]

    def asText: Option[String]

    def asCData: Option[String]

    def isWhitespace: Boolean = node.asAtom.exists(_.trim.isEmpty)

    def isCharacters: Boolean = node.asAtom.exists(_.trim.nonEmpty)
    
    def getText: String = node
      .asAtom
      .orElse(node.asElement.map(_.getChildren).map(toString))
      .getOrElse("")

  // Element name
  extension (element: Element)
    def getName: String

    /* final */ def localName: String =
      val name: String = element.getName
      val colon: Int = name.lastIndexOf(':')
      if colon < 0 then name else name.substring(colon + 1)

    def rename(name: String): Element

    def isElement(elem: XmlElement): Boolean = element.getName == elem.name
    
    def isA: Boolean = isElement(XmlElement.A)
  
  // Children
  extension (element: Element)
    def getChildren: Nodes

    def setChildren(children: Nodes): Element

    def setText(text: String): Element = element.setChildren(Seq(this.text(text)))

    // Remove markup
    def getTextOpt: Option[String] = Option.when(element.getChildren.nonEmpty)(element.getText)

    def flatMapElements[A](f: Element => Seq[A]): Seq[A] = element
      .getChildren
      .flatMap(_.asElement)
      .flatMap(element => f(element))

    def transform(
      transformElement: Element => Element,
      stopAtCode: Boolean = true
    ): Element =
      def loop(element: Element): Element =
        if stopAtCode && element.getName == "code" then element
        else
          val result: Element = transformElement(element)
          result.setChildren(result.getChildren.map(xml => xml.asElement.fold(xml)(loop)))
      loop(element)

    def gather[A](
      gatherElement: Element => Option[A],
      stopAtCode: Boolean = true
    ): Seq[A] =
      def loop(element: Element): Seq[A] =
        val fromElement: Option[A] = gatherElement(element)
        val fromChildren: Seq[A] =
          if stopAtCode && element.getName == "code" then Seq.empty
          else element.flatMapElements(loop)
        fromElement.toSeq ++ fromChildren
      loop(element)

    def gatherWithContext[A](
      gatherElement: (Element, Option[Element]) => Option[A],
      isContext: Element => Boolean,
      stopAtCode: Boolean = true
    ): Seq[A] =
      def loop(element: Element, context: Option[Element]): Seq[A] =
        val fromElement: Option[A] = gatherElement(element, context)
        val fromChildren: Seq[A] =
          if stopAtCode && element.getName == "code" then Seq.empty
          else
            val contextNew: Option[Element] = if isContext(element) then Some(element) else context
            element.flatMapElements(loop(_, contextNew))
        fromElement.toSeq ++ fromChildren
      loop(element, None)

    def gatherWithParent[A](
      gatherElement: (Element, Option[Element]) => Option[A],
      stopAtCode: Boolean = true
    ): Seq[A] =
      element.gatherWithContext(gatherElement, _ => true, stopAtCode)

  // For ScalaXML, I had to deal with the namespace, and had getAttributes(parent) parameter:
  //    val parentNamespaces: Seq[Namespace] = parent.fold[Seq[Namespace]](Seq.empty)(Namespace.getAll)
  //    Namespace.getAll(element).filterNot(parentNamespaces.contains).map(_.attributeValue) ++
  //    Attribute.get(element).filterNot(_.value.isEmpty)

  // Attributes
  extension (element: Element)
    def getAttributes: Seq[(String, String)]

    def setAttributes(attributes: Seq[(String, String)]): Element

    def get(attribute: XmlAttribute): Option[String] =
      get(attribute.name)

    def get(attribute: String): Option[String] =
      element.getAttributes.find(_._1 == attribute).map(_._2)

    def set(attribute: XmlAttribute, value: String): Element =
      set(attribute.name, value)

    def set(attribute: String, value: String): Element =
      val otherAttributes: Seq[(String, String)] = element.getAttributes.filterNot(_._1 == attribute)
      element.setAttributes(
        if value.nonEmpty
        then otherAttributes.appended(attribute -> value)
        else otherAttributes
      )

    def set(attribute: XmlAttribute, value: Option[String]): Element =
      set(attribute.name, value)

    def set(attribute: String, value: Option[String]): Element =
      value.fold(element)(element.set(attribute, _))

    def getId: Option[String] = get(XmlAttribute.Id)

    def setId(value: String): Element = set(XmlAttribute.Id, value)

    def setId(value: Option[String]): Element = set(XmlAttribute.Id, value)

    def copyXmlId: Element =
      if element.getId.exists(_.nonEmpty)
      then element
      else element.setId(element.get(XmlAttribute.XmlId).filter(_.nonEmpty))
    
    def getHref: Option[String] = get(XmlAttribute.Href)
    
    def setHref(value: String): Element = set(XmlAttribute.Href, value)

  // HTML 'class' attribute
  extension (element: Element)
    def getClasses: Seq[String] = element
      .get(HtmlClass)
      .fold(Seq.empty): element =>
        element
          .split(' ')
          .toIndexedSeq
          .map(_.trim)
          .filterNot(_.isEmpty)

    def setClasses(values: Seq[String]): Element =
      element.set(HtmlClass, values.mkString(" "))

    def has(htmlClass: HtmlClass): Boolean = hasClass(htmlClass.name)
    
    def hasClass(htmlClass: String): Boolean = element.getClasses.contains(htmlClass)

    def add(htmlClass: Option[HtmlClass]): Element =
      htmlClass.fold(element)(element.add)

    def add(htmlClass: HtmlClass): Element =
      addClass(htmlClass.name)

    def addClass(htmlClass: String): Element =
      val list = element.getClasses
      if list.contains(htmlClass)
      then element
      else element.setClasses(list.appended(htmlClass))

    def getPrefixedClasses(prefix: String): Seq[String] = element
      .getClasses
      .filter(_.startsWith(s"$prefix-"))
      .map(_.substring(prefix.length + 1))
