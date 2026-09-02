package org.podval.xml

object XmlDialect:
  object Plain extends XmlDialect()

// Describes how to write an XML dialect.
open class XmlDialect(
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
