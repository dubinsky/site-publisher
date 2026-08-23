package org.podval.tools.publish.markup

import org.podval.xml.{HtmlClass, HtmlElement, Xml, XmlAttribute}

// Details of the footnote internal representation.
object Footnote:
  private object CorrelationId extends XmlAttribute("footnoteCorrelationId")

  private object LinkClass extends HtmlClass("footnote-link")

  private object BodyClass extends HtmlClass("footnote")

  private object BackLinkClass extends HtmlClass("footnote-backlink")

  val tip: Tip = Tip("footnote")

  def isLink(element: Xml.Element): Boolean = element.has(LinkClass)

  def isBody(element: Xml.Element): Boolean = element.has(BodyClass)

  def getCorrelationId(element: Xml.Element): String = element.get(CorrelationId).get

  // Note: footnote link will end up as an <a>, but the stub is not -
  // to avoid it being assigned an id and getting resolved ;)
  def link(correlationId: String): Xml.Element = Xml
    .element("span")
    .add(LinkClass)
    .set(CorrelationId, correlationId)
  
  def body(correlationId: String, content: Xml.Nodes): Xml.Element = Xml
    .element("span")
    .add(BodyClass)
    .set(CorrelationId, correlationId)
    .setChildren(content)

final class Footnote(
  val correlationId: String,
  val number: Int,
  val nodes: Xml.Nodes
):
  private def linkId: String = s"_footnote_src_$number"
  private def bodyId: String = s"_footnote_$number"

  def link: Xml.Element = Xml
    .element(HtmlElement.A)
    .add(Footnote.LinkClass)
    .setId(linkId)
    .setHref(s"#$bodyId")
    .setText(number.toString)

  def body: Xml.Element = Xml
    .element("span")
    .add(Footnote.BodyClass)
    .setId(bodyId)
    .setChildren(backLink +: nodes)

  private def backLink: Xml.Element = Xml
    .element(HtmlElement.A)
    .add(Footnote.BackLinkClass)
    .setHref(s"#$linkId")
    .setText(number.toString)
