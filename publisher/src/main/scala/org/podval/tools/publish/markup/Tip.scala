package org.podval.tools.publish.markup

import org.podval.xml.{CssClass, Xml, XmlAttribute, XmlElement}
import Xml.given

final class Tip(prefix: String):
  object RefClass extends CssClass(s"$prefix-ref")
  object TipClass extends CssClass(s"$prefix-tip")

  /** Wrap the link and tip as siblings in `span.{prefix}-ref` (CSS hover target).
    * `aria-describedby` stays on the `<a>`. */
  def attachTip(link: Xml.Element, definition: Xml.Nodes): Xml.Element =
    var tip: Xml.Element = Xml
      .element(XmlElement.Span)
      .add(TipClass)
      .setChildren(definition)
    var wrappedLink: Xml.Element = link
    link.getId.foreach: id =>
      val tipId: String = s"$id-tip"
      tip = tip.setId(tipId).set(XmlAttribute.Role, "tooltip")
      wrappedLink = wrappedLink.set("aria-describedby", tipId)
    Xml
      .element(XmlElement.Span)
      .add(RefClass)
      .setChildren(Seq(wrappedLink, tip))

  def isRef(element: Xml.Element): Boolean = element.has(RefClass)

  def isTip(node: Xml.Node): Boolean =
    node.asElement.exists(_.has(TipClass))
