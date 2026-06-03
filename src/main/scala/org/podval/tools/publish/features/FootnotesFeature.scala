package org.podval.tools.publish.features

import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.{HtmlClass, Xml, XmlElement}
import zio.blocks.chunk.Chunk

object FootnotesFeature extends Feature(
  // Note: transform this last, so that everything that was to be converted to a footnote had:
  transformPriority = 100
):

  override def transform(
    element: Xml.Element,
    context: Feature.TransformContext
  ): Xml.Element =
    var xml: Xml.Element = element

    // Retrieve footnote bodies
    val footnoteBodies: Map[String, Chunk[Xml.Node]] = context.xmlDialect.gather(xml, element =>
      if !element.has(Footnotes.BodyClass)
      then None
      else element.get(Footnotes.CorrelationId).map(_ -> element.getChildren)
    ).toMap

    // Replace footnotes with link stubs
    xml = context.xmlDialect.transform(xml, element =>
      element.get(Footnotes.CorrelationId).fold(element)(Footnotes.linkStub)
    )

    // Remove body stubs
    xml = context.xmlDialect.transform(xml, element =>
      element.setChildren(element
        .getChildren
        .filterNot(_.asElement.fold(false)(child =>
          child.has(Footnotes.BodyClass) ||
          // FlexMark FootnotesExtension footnotes 'div'
          child.getName == "div" && child.has(HtmlClass("footnotes"))
        ))
      )
    )

    // Number the footnotes
    val footnoteNumbers: IdGenerator = IdGenerator("")
    var footnotesToAdd: Chunk[Xml.Element] = Chunk.empty

    xml = context.xmlDialect.transform(xml, element =>
      element.get(Footnotes.CorrelationId).fold(element): correlationId =>
        val footnoteNumber: String = footnoteNumbers.generate()
        // TODO error when not found:
        footnoteBodies.get(correlationId).foreach: footnoteBody =>
          footnotesToAdd = footnotesToAdd.appended(Footnotes.body(footnoteNumber, footnoteBody))
        Footnotes.link(footnoteNumber)
      )

    // Add footnotes 'div'
    val footnotesDiv: Xml.Element = Xml
      .element(XmlElement("div"))
      .add(HtmlClass("footnotes"))
      .setChildren(footnotesToAdd)

    if footnotesToAdd.isEmpty
    then xml
    else xml.setChildren(xml.getChildren :+ footnotesDiv)


