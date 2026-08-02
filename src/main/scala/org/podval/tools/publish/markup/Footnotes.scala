package org.podval.tools.publish.markup

import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.{HtmlClass, HtmlElement, Xml, XmlAttribute, XmlDialect}
import zio.blocks.chunk.Chunk

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

  def footnoteBodies(xml: Xml.Element, xmlDialect: XmlDialect): Map[String, Chunk[Xml.Node]] = 
    xmlDialect.gather(xml, element =>
      if !element.has(BodyClass)
      then None
      else getCorrelationId(element).map(_ -> element.getChildren)
    ).toMap

  def removeFootnoteBodies(xml: Xml.Element, markup: Markup): Xml.Element =
    markup.xmlDialect.transform(xml, element =>
      element.setChildren(element
        .getChildren
        .filterNot(_.asElement.fold(false)(child =>
          val remove: Boolean =
            child.has(Footnotes.BodyClass) ||
              markup.isSpuriousFootnotesDiv(element)
          // TODO AsciiDoc footnotes div does not get removed!
          if remove then
            val x = 0
          remove
        ))
      )
    )
    
  def transformFootnotes(
    element: Xml.Element,
    footnoteBodies: Map[String, Chunk[Xml.Node]],
    xmlDialect: XmlDialect
  ): Xml.Element =
    val footnoteNumbers: IdGenerator = IdGenerator("")

    // Number the footnotes
    var footnotesToAdd: Chunk[Xml.Element] = Chunk.empty

    val xml: Xml.Element = xmlDialect.transform(element, element =>
      Footnotes.getCorrelationId(element).fold(element): correlationId =>
        val footnoteNumber: String = footnoteNumbers.generate()
        // TODO error when not found:
        footnoteBodies.get(correlationId).foreach: footnoteBody =>
          footnotesToAdd = footnotesToAdd.appended(Footnotes.body(footnoteNumber, footnoteBody))
        Footnotes.link(footnoteNumber)
    )

    // Add footnotes 'div'
    val footnotesDiv: Xml.Element = Xml
      .element("div")
      .addClass("footnotes")
      .setChildren(footnotesToAdd)

    if footnotesToAdd.isEmpty
    then xml
    else xml.setChildren(xml.getChildren :+ footnotesDiv)

