package org.podval.tools.publish.features

import org.podval.xml.{HtmlAttribute, HtmlClass, HtmlElement, Xml, XmlAttribute, XmlElement}

// TODO footnotes placed at the end of elements like table, not the overall end?
// TODO how do multi-level footnotes look?
object Footnotes:
  object CorrelationId extends XmlAttribute("footnoteCorrelationId")

  object LinkClass extends HtmlClass("footnote-link")
  object BodyClass extends HtmlClass("footnote")
  private object BackLinkClass extends HtmlClass("footnote-backlink")

  private def footnoteId(footnoteNumber: String): String = s"_footnote_src_$footnoteNumber"
  private def footnoteBodyId(footnoteNumber: String): String = s"_footnote_$footnoteNumber"

  def linkStub(correlationId: String): Xml.Element = Xml
    .element(HtmlElement.A)
    .add(LinkClass)
    .set(CorrelationId, correlationId)

  def link(footnoteNumber: String): Xml.Element = Xml
    .element(HtmlElement.A)
    .add(LinkClass)
    .set(XmlAttribute.Id, footnoteId(footnoteNumber))
    .set(HtmlAttribute.Href, s"#${footnoteBodyId(footnoteNumber)}")
    .setText(footnoteNumber)

  def bodyStub(correlationId: String, content: Xml.Nodes): Xml.Element = Xml
    .element(XmlElement("span"))
    .add(BodyClass)
    .set(CorrelationId, correlationId)
    .setChildren(content)

  def body(
    footnoteNumber: String,
    footnoteBody: Xml.Nodes
  ): Xml.Element = Xml
    .element(XmlElement("span"))
    .add(BodyClass)
    .set(XmlAttribute.Id, Footnotes.footnoteBodyId(footnoteNumber))
    .setChildren(Footnotes.backLink(footnoteNumber) +: footnoteBody)

  private def backLink(footnoteNumber: String): Xml.Element = Xml
    .element(HtmlElement.A)
    .add(BackLinkClass)
    .set(HtmlAttribute.Href, s"#${footnoteId(footnoteNumber)}")
    .setText(footnoteNumber)
