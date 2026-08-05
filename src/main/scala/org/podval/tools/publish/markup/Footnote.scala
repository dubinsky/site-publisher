package org.podval.tools.publish.markup

import org.podval.xml.{HtmlClass, HtmlElement, Xml, XmlAttribute, XmlDialect}
import zio.blocks.chunk.Chunk

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

// TODO footnotes placed at the end of elements like table, not the overall end?
// TODO how do multi-level footnotes look?
// Details of the footnote internal representation.
object Footnote:
  private object CorrelationId extends XmlAttribute("footnoteCorrelationId")

  private object LinkClass extends HtmlClass("footnote-link")

  private object BodyClass extends HtmlClass("footnote")

  private object BackLinkClass extends HtmlClass("footnote-backlink")

  // Note: footnote link will end up as an <a>, but the stub is not -
  // to avoid it being assigned an id and getting resolved ;)
  def link(correlationId: String): Xml.Element = Xml
    .element("span")
    .add(LinkClass)
    .set(CorrelationId, correlationId)

  def links(xml: Xml.Element, xmlDialect: XmlDialect): Chunk[String] =
    xmlDialect.gather(xml, element =>
      Option.when(element.has(LinkClass))(element.get(CorrelationId).get)
    )

  def body(correlationId: String, content: Xml.Nodes): Xml.Element = Xml
    .element("span")
    .add(BodyClass)
    .set(CorrelationId, correlationId)
    .setChildren(content)

  def bodies(xml: Xml.Element, xmlDialect: XmlDialect): Seq[(String, Xml.Nodes)] =
    xmlDialect.gather(xml, element =>
      Option.when(element.has(BodyClass))(element.get(CorrelationId).get -> element.getChildren)
    )

  def removeBodies(xml: Xml.Element, markup: Markup): Xml.Element =
    markup.xmlDialect.transform(xml, element =>
      element.setChildren(element
        .getChildren
        .filterNot(_.asElement.fold(false)(child =>
          child.has(BodyClass) ||
            markup.isSpuriousFootnotesDiv(child)
        ))
      )
    )

  def convertLinks(
    xml: Xml.Element,
    footnotes: Map[String, Footnote],
    xmlDialect: XmlDialect
  ): Xml.Element =
    xmlDialect.transform(xml, element => Option
      .when(element.has(LinkClass))(
        footnotes(element.get(CorrelationId).get).link
      )
      .getOrElse(element)
    )

  def addFootnotesDiv(
    xml: Xml.Element,
    footnotes: Chunk[Footnote]
  ): Xml.Element =
    val footnotesDiv: Xml.Element = Xml
      .element("div")
      .addClass("footnotes")
      .setChildren(footnotes.map(_.body))

    if footnotes.isEmpty
    then xml
    else xml.setChildren(xml.getChildren :+ footnotesDiv)
