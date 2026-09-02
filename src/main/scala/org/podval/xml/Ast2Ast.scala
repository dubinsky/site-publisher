package org.podval.xml

// Note: I do not see any reason to recognize special elements (like 'script') or attributes (like 'hidden')
// when converting to Html...
// ZIO Blocks HTML does not support comments nor processing instructions
abstract class Ast2Ast[FromElement, ToElement](from: XmlAst[FromElement], to: XmlAst[ToElement]):
  def convert(element: FromElement): ToElement =
    val named: ToElement = to.element(from.getName(element))
    val attributed: ToElement = to.setAttributes(named)(from.getAttributes(element))
    to.setChildren(attributed)(convertChildren(from.getChildren(element)))

  private def convertChildren(children: from.Nodes): to.Nodes =
    val buf = List.newBuilder[to.Node]
    children.foreach: child =>
      from.asElement(child).map(convert)
        .orElse(from.asCData(child).map(to.cdata))
        .orElse(from.asAtom(child).map(to.text))
        .foreach(node => buf += node)
    buf.result()

object Ast2Ast:
  object XmlToHtml extends Ast2Ast(Xml, Html)
