package org.podval.tools.publish.markup

import org.podval.xml.Xml
import org.podval.xml.XmlNode.flatMapNodes

private[markup] object DialectRewrite:
  def walk(
    converted: Xml.Element,
    parent: Option[Xml.Element],
    rewriteChild: Xml.Element => Xml.Rewrite,
    emitWhole: Xml.Element => Option[Xml.Nodes],
    converters: Seq[Xml.Element => Xml.Element],
    convertCode: Xml.Element => Option[Xml.Nodes],
    ifNotCode: Xml.Element => Xml.Nodes
  ): Xml.Rewrite =
    val children: Xml.Nodes = converted.getChildren.flatMapNodes: node =>
      node.asElement match
        case Some(child) =>
          rewriteChild(child) match
            case Xml.Rewrite.Keep(result) => Seq(result)
            case Xml.Rewrite.Emit(nodes) => nodes
            case Xml.Rewrite.Replace(nodes) => nodes
        case None =>
          Seq(node)
    val ready: Xml.Element = converted.setChildren(children)
    emitWhole(ready) match
      case Some(nodes) =>
        Xml.Rewrite.Emit(nodes)
      case None =>
        val result: Xml.Element = converters.foldLeft(ready)((el, step) => step(el))
        // Children are already finished. A `pre` wrapper is emitted as-is;
        // `code` inside an existing `pre` is not wrapped again.
        // Those two paths do not lift a facsimile anchor out of a link.
        if parent.exists(_.isNamed("pre")) then Xml.Rewrite.Emit(Seq(result))
        else convertCode(ready) match
          case Some(nodes) => Xml.Rewrite.Emit(nodes)
          case None => Xml.Rewrite.Emit(ifNotCode(result))
