package org.podval.xml

import zio.blocks.chunk.Chunk

object XmlDialect:
  object Plain extends XmlDialect(root = Set.empty)

// Describes an XML dialect.
open class XmlDialect(
  // identification
  val root: Set[String],

  // traversal stop
  val stop: Set[String] = Set.empty,

  // writing
  val preformat: Set[String] = Set.empty,
  val stack: Set[String] = Set.empty,
  val unStack: Set[String] = Set.empty,
  val nest: Set[String] = Set.empty,
  val break: Set[String] = Set.empty,
  val cling: Set[String] = Set.empty,

  //  if allowEmptyElements || keepEmptyElements.contains(name.localName)
  //  Some elements are mis-processed when they are empty, e.g. <script .../> ...
  //  ... except, some elements are mis-processed when they *are* non-empty (e.g., <br>),
  //  and in general, it's weird to expand the elements that are always empty...
  val selfClose: Set[String] = Set.empty,

  // TODO do not double-encode what you did not decode ;)
  val encodeXmlSpecials: Boolean = false
):
  def plus(other: XmlDialect): XmlDialect = XmlDialect(
    root = root ++ other.root,
    stop = stop ++ other.stop,
    preformat = preformat ++ other.preformat,
    stack = stack ++ other.stack,
    unStack = unStack ++ other.unStack,
    nest = nest ++ other.nest,
    break = break ++ other.break,
    cling = cling ++ other.cling,
    selfClose = selfClose ++ other.selfClose,
    encodeXmlSpecials = encodeXmlSpecials || other.encodeXmlSpecials
  )

  def render[Element: XmlAst](
    element: Element,
    width: Int = XmlWriter.widthDefault
  ): String = XmlWriter.render(this, element, width)

  private def stop[Element: XmlAst](element: Element): Boolean =
    stop.contains(element.getName)

  def transform[Element: XmlAst](
    element: Element,
    transformElement: Element => Element
  ): Element =
    def loop(element: Element): Element = if stop(element) then element else
      val result: Element = transformElement(element)
      result.setChildren(result.getChildren.map(xml => xml.asElement.fold(xml)(loop)))

    loop(element)

  final def gather[A, Element: XmlAst](
    element: Element,
    gatherElement: Element => Option[A]
  ): Chunk[A] =
    def loop(element: Element): Chunk[A] =
      val fromElement: Option[A] = gatherElement(element)
      val fromChildren: Chunk[A] = if stop(element) then Chunk.empty else
        element.flatMapElements(loop)
      Chunk.from(fromElement) ++ fromChildren

    loop(element)

  final def gatherWithParents[A, Element: XmlAst](
    element: Element,
    gatherElement: (Element, Seq[Element]) => Option[A]
  ): Seq[A] =
    def loop(element: Element, parents: Seq[Element]): Chunk[A] =
      val fromElement: Option[A] = gatherElement(element, parents)
      val fromChildren: Chunk[A] = if stop(element) then Chunk.empty else
        val parentsNew: Seq[Element] = element +: parents
        element.flatMapElements(loop(_, parentsNew))

      Chunk.from(fromElement) ++ fromChildren

    loop(element, Seq.empty)

