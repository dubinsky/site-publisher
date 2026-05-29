package org.podval.tools.publish

import org.podval.xml.Xml
import zio.blocks.chunk.Chunk

// TODO footnotes placed at the end of elements like table, not the overall end?
final class Footnotes(val footnotes: Map[String, Chunk[Xml.Xml]]):
  def body(
    footnoteNumber: String,
    correlationId: String
  ): Xml.Element =
    var result: Xml.Element = Xml.element("span")
    result = Footnotes.BodyClass.add(result)
    result = Xml.Id.set(result, Footnotes.footnoteBodyId(footnoteNumber))
    result = Xml.setChildren(result, Footnotes.backLink(footnoteNumber) +: footnotes(correlationId))
    result

object Footnotes:
  object CorrelationId extends Xml.Attribute("footnoteCorrelationId")

  private object LinkClass extends Xml.ClassName("footnote-link")
  object BodyClass extends Xml.ClassName("footnote")
  private object BackLinkClass extends Xml.ClassName("footnote-backlink")

  private def footnoteId(footnoteNumber: String): String = s"_footnote_src_$footnoteNumber"
  private def footnoteBodyId(footnoteNumber: String): String = s"_footnote_$footnoteNumber"

  def linkStub(correlationId: String): Xml.Element =
    var result: Xml.Element = Xml.A.mk
    result = LinkClass.add(result)
    result = CorrelationId.set(result, correlationId)
    result

  def bodyStub(correlationId: String, content: Chunk[Xml.Xml]): Xml.Element =
    var result: Xml.Element = Xml.element("span")
    result = BodyClass.add(result)
    result = CorrelationId.set(result, correlationId)
    result = Xml.setChildren(result, content)
    result

  def link(footnoteNumber: String): Xml.Element =
    var result: Xml.Element = Xml.A.mk
    result = LinkClass.add(result)
    result = Xml.Id.set(result, footnoteId(footnoteNumber))
    result = Xml.Href.set(result, s"#${footnoteBodyId(footnoteNumber)}")
    result = Xml.setText(result, footnoteNumber)
    result
  
  private def backLink(footnoteNumber: String): Xml.Element =
    var result: Xml.Element = Xml.A.mk
    result = BackLinkClass.add(result)
    result = Xml.Href.set(result, s"#${footnoteId(footnoteNumber)}")
    result = Xml.setText(result, footnoteNumber)
    result
