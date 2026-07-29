package org.podval.tools.publish.markup

import org.podval.tools.publish.markup.Footnotes
import org.podval.tools.publish.page.PageSource
import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.Xml
import zio.blocks.chunk.Chunk

// TODO split up; carry footnotes in the PageContent...
final class FootnotesTransformer(markup: Markup):
  def transform(
    element: Xml.Element,
    source: PageSource
  ): Xml.Element =
    var xml: Xml.Element = element

    // Retrieve footnote bodies
    val footnoteBodies: Map[String, Chunk[Xml.Node]] = source.xmlDialect.gather(xml, element =>
      if !element.has(Footnotes.BodyClass)
      then None
      else Footnotes.getCorrelationId(element).map(_ -> element.getChildren)
    ).toMap

    // Replace footnotes with link stubs
    xml = source.xmlDialect.transform(xml, element =>
      Footnotes.getCorrelationId(element).fold(element)(Footnotes.linkStub)
    )

    // Remove body stubs
    xml = source.xmlDialect.transform(xml, element =>
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

    // Number the footnotes
    val footnoteNumbers: IdGenerator = IdGenerator("")
    var footnotesToAdd: Chunk[Xml.Element] = Chunk.empty

    xml = source.xmlDialect.transform(xml, element =>
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
