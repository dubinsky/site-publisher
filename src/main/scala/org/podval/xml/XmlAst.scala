package org.podval.xml

import zio.blocks.chunk.Chunk

// AST that represents XML and provides operations on it;
// abstracts over the underlying representation:
// - ZIO Blocks XML
// - ZIO Blocks HTML
// - potentially Scala XML
// - potentially DOM
trait XmlAst[ELEMENT]:
  final type Element = ELEMENT

  type Node >: Element

  final type Nodes = Chunk[Node]

  def text(text: String): Node

  def element(elem: XmlElement): Element

  final def toString(nodes: Nodes): String = nodes.map(_.getText).mkString(" ")

  // Conversions
  extension (node: Node)
    def asElement: Option[Element]
    def asAtom: Option[String]
    def asText: Option[String]
    def getText: String = node
      .asAtom
      .orElse(node.asElement.map(_.getChildren).map(toString))
      .getOrElse("")

  // Element name
  extension (element: Element)
    def getName: String

    def isElement(elem: XmlElement): Boolean = element.getName == elem.name

    def rename(name: String): Element

  // Children
  extension (element: Element)
    def getChildren: Nodes

    def setChildren(children: Nodes): Element

    def setText(text: String): Element = element.setChildren(Chunk(this.text(text)))

    // Remove markup
    def getTextOpt: Option[String] = Option.when(element.getChildren.nonEmpty)(element.getText)

    def flatMapElements[A](f: Element => Chunk[A]): Chunk[A] = element
      .getChildren
      .flatMap(_.asElement)
      .flatMap(element => f(element))

  // For ScalaXML, I had to deal with the namespace, and had getAttributes(parent) parameter:
  //    val parentNamespaces: Seq[Namespace] = parent.fold[Seq[Namespace]](Seq.empty)(Namespace.getAll)
  //    Namespace.getAll(element).filterNot(parentNamespaces.contains).map(_.attributeValue) ++
  //    Attribute.get(element).filterNot(_.value.isEmpty)

  // Attributes
  extension (element: Element)
    def getAttributes: Chunk[(String, String)]

    def setAttributes(attributes: Chunk[(String, String)]): Element

    def get(attribute: XmlAttribute): Option[String] =
      element.getAttributes.find(_._1 == attribute.name).map(_._2)

    def set(attribute: XmlAttribute, value: String): Element =
      element.setAttributes(element.getAttributes.filterNot(_._1 == attribute.name).appended(attribute.name -> value))

    def set(attribute: XmlAttribute, value: Option[String]): Element =
      value.fold(element)(element.set(attribute, _))

  // HTML 'class' attribute
  extension (element: Element)
    private def getClasses: Chunk[String] = element
      .get(HtmlClass)
      .fold(Chunk.empty): element =>
        Chunk.from(element.split(' '))
          .map(_.trim)
          .filterNot(_.isEmpty)

    private def setClasses(values: Chunk[String]): Element =
      element.set(HtmlClass, values.mkString(" "))

    def has(htmlClass: HtmlClass): Boolean = element.getClasses.contains(htmlClass.name)

    def add(htmlClass: Option[HtmlClass]): Element =
      htmlClass.fold(element)(element.add)
      
    def add(htmlClass: HtmlClass): Element =
      val list = element.getClasses
      if list.contains(htmlClass.name)
      then element
      else element.setClasses(list.appended(htmlClass.name))

    def getPrefixedClasses(prefix: String): Chunk[String] = element
      .getClasses
      .filter(_.startsWith(s"$prefix-"))
      .map(_.substring(prefix.length + 1))
