package org.podval.tools.publish.markup

import org.podval.tools.publish.markup.{Footnotes, Transformer}
import org.podval.tools.publish.page.PageSource
import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.Xml
import zio.blocks.chunk.Chunk

final class FootnotesTransformer(
  footnoteBodies: Map[String, Chunk[Xml.Node]],
  ids: IdGenerator,
  source: PageSource
) extends Transformer:
  override def transform(element: Xml.Element): Xml.Element =
    // Number the footnotes
    var footnotesToAdd: Chunk[Xml.Element] = Chunk.empty

    val xml: Xml.Element = source.xmlDialect.transform(element, element =>
      Footnotes.getCorrelationId(element).fold(element): correlationId =>
        val footnoteNumber: String = ids.footnoteNumber()
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
