package org.podval.tools.publish.markup

import org.podval.tools.publish.markup.Footnotes
import org.podval.tools.publish.page.PageContent
import org.podval.tools.publish.processor.Transformer
import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.Xml
import zio.blocks.chunk.Chunk

final class FootnotesTransformer extends Transformer(transformsFootnotes = true):
  override def transform(
    element: Xml.Element,
    content: PageContent
  ): Xml.Element =
    var xml: Xml.Element = element

    // Retrieve footnote bodies
    val footnoteBodies: Map[String, Chunk[Xml.Node]] = content.xmlDialect.gather(xml, element =>
      if !Footnotes.isBody(element)
      then None
      else Footnotes.getCorrelationId(element).map(_ -> element.getChildren)
    ).toMap

    // Replace footnotes with link stubs
    xml = content.xmlDialect.transform(xml, element =>
      Footnotes.getCorrelationId(element).fold(element)(Footnotes.linkStub)
    )

    // Remove body stubs
    xml = content.xmlDialect.transform(xml, element =>
      element.setChildren(element
        .getChildren
        .filterNot(_.asElement.fold(false)(child =>
          Footnotes.isBody(child) ||
            // FlexMark FootnotesExtension footnotes 'div'
            child.getName == "div" && child.hasClass("footnotes")
        ))
      )
    )

    // Number the footnotes
    val footnoteNumbers: IdGenerator = IdGenerator("")
    var footnotesToAdd: Chunk[Xml.Element] = Chunk.empty

    xml = content.xmlDialect.transform(xml, element =>
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
