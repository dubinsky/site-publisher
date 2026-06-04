package org.podval.tools.publish.feature

import org.podval.tools.publish.page.PageSource
import org.podval.tools.publish.processor.{Feature, Transformer}
import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.{HtmlClass, Xml, XmlElement}
import zio.blocks.chunk.Chunk

final class FootnotesFeature extends Feature(
  transformer = Some(FootnotesFeature.FootnotesTransformer())
)

object FootnotesFeature:
  private final class FootnotesTransformer extends Transformer:
    override def runLast: Boolean = true

    override def transform(
      element: Xml.Element,
      pageSource: PageSource
    ): Xml.Element =
      var xml: Xml.Element = element

      // Retrieve footnote bodies
      val footnoteBodies: Map[String, Chunk[Xml.Node]] = pageSource.xmlDialect.gather(xml, element =>
        if !Footnotes.isBody(element)
        then None
        else Footnotes.getCorrelationId(element).map(_ -> element.getChildren)
      ).toMap

      // Replace footnotes with link stubs
      xml = pageSource.xmlDialect.transform(xml, element =>
        Footnotes.getCorrelationId(element).fold(element)(Footnotes.linkStub)
      )

      // Remove body stubs
      xml = pageSource.xmlDialect.transform(xml, element =>
        element.setChildren(element
          .getChildren
          .filterNot(_.asElement.fold(false)(child =>
            Footnotes.isBody(child) ||
            // FlexMark FootnotesExtension footnotes 'div'
            child.getName == "div" && child.has(HtmlClass("footnotes"))
          ))
        )
      )

      // Number the footnotes
      val footnoteNumbers: IdGenerator = IdGenerator("")
      var footnotesToAdd: Chunk[Xml.Element] = Chunk.empty

      xml = pageSource.xmlDialect.transform(xml, element =>
        Footnotes.getCorrelationId(element).fold(element): correlationId =>
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


