package org.podval.xml

// Note: I do not see any reason to recognize special elements (like 'script') or attributes (like 'hidden')
// when converting to Html...
// ZIO Blocks HTML does not support comments nor processing instructions
abstract class Ast2Ast[FromElement, ToElement](from: XmlAst[FromElement], to: XmlAst[ToElement]):
  def convert(element: from.Element): to.Element =
    var result: to.Element = to.element(from.getName(element))
    result = to.setAttributes(result)(from.getAttributes(element))
    result = to.setChildren(result)(convertChildren(from.getChildren(element)))
    result

  private def convertChildren(children: from.Nodes): to.Nodes = children
    .foldLeft(Seq.empty[to.Node]): (acc, child) =>
      child.asElement.map(convert)
        .orElse(child.asAtom.map(to.text))
        .fold(acc)(acc :+ _)

object Ast2Ast:
  object Xml2Html extends Ast2Ast(Xml, Html)
  
  