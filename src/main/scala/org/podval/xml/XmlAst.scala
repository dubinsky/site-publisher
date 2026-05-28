package org.podval.xml

import zio.blocks.chunk.Chunk

abstract class XmlAst:
  type Xml
  
  type Element <: Xml

  // Remove markup
  final def toStringOpt(element: Element): Option[String] =
    Option.when(children(element).nonEmpty)(toString(element))

  final def toString(xml: Xml): String = asAtom(xml)
    .orElse(asElement(xml).map(children).map(toString))
    .getOrElse("")

  final def toString(xml: Chunk[Xml]): String = xml.map(toString).mkString(" ")
  
  def asElement(xml: Xml): Option[Element]
  def name(element: Element): String
  def rename(element: Element, name: String): Element
  def element(name: String): Element

  // TODO deal with the namespaces - and defaults?!
  //    val parentNamespaces: Seq[Namespace] = parent.fold[Seq[Namespace]](Seq.empty)(Namespace.getAll)
  //    Namespace.getAll(element).filterNot(parentNamespaces.contains).map(_.attributeValue) ++
  //    Attribute.get(element).filterNot(_.value.isEmpty)
  def attributes(element: Element, parent: Option[Element]): Chunk[(String, String)]
  def attributes(element: Element): Chunk[(String, String)]
  def setAttributes(element: Element, attributes: Chunk[(String, String)]): Element
  final def getAttribute(element: Element, name: String): Option[String] = attributes(element).find(_._1 == name).map(_._2)
  def setAttribute(element: Element, name: String, value: String): Element
  
  def children(element: Element): Chunk[Xml]
  def setChildren(element: Element, children: Chunk[Xml]): Element
  final def setText(element: Element, text: String): Element = setChildren(element, Chunk(mkText(text)))

  def mkText(text: String): Xml
  def asAtom(xml: Xml): Option[String]
  def asText(xml: Xml): Option[String]

  def transform(
    element: Element,
    // TODO change to:  stop: XmlAst => Element => Boolean
    stop: Element => Boolean,
    transformElement: Element => Element
  ): Element =
    def loop(element: Element): Element = if stop(element) then element else
      val result: Element = transformElement(element)
      setChildren(result, children(result).map(xml => asElement(xml).fold(xml)(loop)))

    loop(element)

  final def gather[A](
    element: Element,
    stop: Element => Boolean,
    gatherElement: Element => Option[A]
  ): Seq[A] =
    def loop(element: Element): Seq[A] =
      val fromElement: Option[A] = gatherElement(element)
      val fromChildren: Seq[A] = if stop(element) then Seq.empty else
        flatMapChildren(element, element => loop(element))

      fromElement.toSeq ++ fromChildren

    loop(element)

  final def gatherWithParents[A](
    element: Element,
    stop: Element => Boolean,
    gatherElement: (Element, Seq[Element]) => Option[A]
  ): Seq[A] =
    def loop(element: Element, parents: Seq[Element]): Seq[A] =
      val fromElement: Option[A] = gatherElement(element, parents)
      val fromChildren: Seq[A] = if stop(element) then Seq.empty else
        val parentsNew: Seq[Element] = element +: parents
        flatMapChildren(element, element => loop(element, parentsNew))

      fromElement.toSeq ++ fromChildren

    loop(element, Seq.empty)

  def flatMapChildren[A](element: Element, f: Element => Seq[A]): Seq[A] = children(element)
    .flatMap(asElement)
    .flatMap(element => f(element))

  sealed abstract class Elem(val elementName: String):
    def is(element: Element): Boolean = name(element) == elementName

  object A extends Elem("a")

  object Code extends Elem("code")

  abstract class Attribute(val attributeName: String):
    final def get(element: Element): Option[String] = getAttribute(element, attributeName)
    final def set(element: Element, value: String): Element = setAttribute(element, attributeName, value)

  object Id extends Attribute("id"):
    def toId(text: String): String = text.trim.replace(' ', '-')

  object Href extends Attribute("href")

  object ClassName extends Attribute("class"):
    def set(element: Element, values: List[String]): Element = set(element, values.mkString(" "))
    def has(element: Element, name: String): Boolean = getList(element).contains(name)

    private def getList(element: Element): List[String] = get(element).fold(List.empty)(_
      .split(' ')
      .toList
      .map(_.trim)
      .filterNot(_.isEmpty)
    )

    def getStartsWith(element: Element, prefix: String): List[String] = getList(element)
      .filter(_.startsWith(s"$prefix-"))
      .map(_.substring(prefix.length + 1))

    def add(element: Element, name: String): Element =
      val list = getList(element)
      if list.contains(name)
      then element
      else set(element, list.appended(name))

  abstract class ClassName(val name: String):
    final def add(element: Element): Element = ClassName.add(element, name)
    final def has(element: Element): Boolean = ClassName.has(element, name)

  abstract class ClassNamePrefix(val prefix: String):
    final def add(element: Element, name: String): Element = ClassName.add(element, s"$prefix-$name")
    final def get(element: Element): List[String] = ClassName.getStartsWith(element, prefix)
