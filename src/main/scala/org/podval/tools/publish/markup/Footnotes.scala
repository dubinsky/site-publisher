package org.podval.tools.publish.markup

import org.podval.xml.{HtmlClass, HtmlElement, Xml, XmlAttribute}

// TODO footnotes placed at the end of elements like table, not the overall end?
// TODO how do multi-level footnotes look?
// Details of the footnote internal representation.
object Footnotes:
  private object CorrelationId extends XmlAttribute("footnoteCorrelationId")

  object LinkClass extends HtmlClass("footnote-link")
  
  object BodyClass extends HtmlClass("footnote")
  
  private object BackLinkClass extends HtmlClass("footnote-backlink")

  def getCorrelationId(element: Xml.Element): Option[String] = element.get(CorrelationId)
  def setCorrelationId(element: Xml.Element, value: String): Xml.Element = element.set(CorrelationId, value)
  
  private def footnoteId(footnoteNumber: String): String = s"_footnote_src_$footnoteNumber"
  private def footnoteBodyId(footnoteNumber: String): String = s"_footnote_$footnoteNumber"

  def isLink(element: Xml.Element): Boolean = element.has(LinkClass)
  
  def isBody(element: Xml.Element): Boolean = element.has(BodyClass)
  
  def linkAndBodyStub(element: Xml.Element, correlationId: String): Xml.Element = element
    .set(CorrelationId, correlationId)
    .add(Footnotes.LinkClass)
    .add(Footnotes.BodyClass)
  
  def linkStub(correlationId: String): Xml.Element = Xml
    .element(HtmlElement.A)
    .add(LinkClass)
    .set(CorrelationId, correlationId)

  def link(footnoteNumber: String): Xml.Element = Xml
    .element(HtmlElement.A)
    .add(LinkClass)
    .setId(footnoteId(footnoteNumber))
    .setHref(s"#${footnoteBodyId(footnoteNumber)}")
    .setText(footnoteNumber)

  def bodyStub(correlationId: String, content: Xml.Nodes): Xml.Element = Xml
    .element("span")
    .add(BodyClass)
    .set(CorrelationId, correlationId)
    .setChildren(content)

  def body(
    footnoteNumber: String,
    footnoteBody: Xml.Nodes
  ): Xml.Element = Xml
    .element("span")
    .add(BodyClass)
    .setId(Footnotes.footnoteBodyId(footnoteNumber))
    .setChildren(Footnotes.backLink(footnoteNumber) +: footnoteBody)

  private def backLink(footnoteNumber: String): Xml.Element = Xml
    .element(HtmlElement.A)
    .add(BackLinkClass)
    .setHref(s"#${footnoteId(footnoteNumber)}")
    .setText(footnoteNumber)
