package org.podval.tools.publish.markup

import org.podval.xml.{HtmlClass, Xml}
import zio.blocks.chunk.Chunk

final class Tip(prefix: String):
  object RefClass extends HtmlClass(s"$prefix-ref")
  object TipClass extends HtmlClass(s"$prefix-tip")

  /** Wrap the link and tip as siblings in `span.{prefix}-ref` (CSS hover target).
    * `aria-describedby` stays on the `<a>`. */
  def attachTip(link: Xml.Element, definition: Xml.Nodes): Xml.Element =
    var tip: Xml.Element = Xml
      .element("span")
      .add(TipClass)
      .setChildren(definition)
    var wrappedLink: Xml.Element = link
    link.getId.foreach: id =>
      val tipId: String = s"$id-tip"
      tip = tip.setId(tipId).set("role", "tooltip")
      wrappedLink = wrappedLink.set("aria-describedby", tipId)
    Xml
      .element("span")
      .add(RefClass)
      .setChildren(Chunk(wrappedLink, tip))

  def isRef(element: Xml.Element): Boolean = element.has(RefClass)

  def isTip(node: Xml.Node): Boolean =
    node.asElement.exists(_.has(TipClass))
