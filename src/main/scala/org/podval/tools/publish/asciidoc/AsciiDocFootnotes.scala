package org.podval.tools.publish.asciidoc

import org.podval.tools.publish.markup.Footnotes
import org.podval.xml.Xml

object AsciiDocFootnotes:
  // From:
  //   <sup class="footnote">[<a id="_footnoteref_N" class="footnote" href="#_footnotedef_N">N</a>]</sup>
  // To:
  //   <a class="footnote-link" footnoteCorrelationId="N"/>
  def convertFootnoteLink(element: Xml.Element): Option[Xml.Element] =
    val isFootnoteLink: Boolean = element.getName == "sup" /* && element.hasClass("footnote") */
    if !isFootnoteLink then None else
      for correlationId: String <- element
        .getChildren
        .flatMap(_.asElement)
        .find(_.hasClass("footnote"))
        .map(_.getText)
      yield
        Footnotes.linkStub(correlationId)

  // From:
  //   <div class="footnote" id="_footnotedef_N"><a href="#_footnoteref_N">N</a>. Footnote Body</div>
  // To:
  //   <span class="footnote" footnoteCorrelationId="N">Footnote Body</span>
  def convertFootnoteBody(element: Xml.Element): Option[Xml.Element] =
    val isFootnoteBody: Boolean = element.getName == "div" && element.hasClass("footnote")
    if !isFootnoteBody then None else
      for correlationId: String <- element
        .getChildren
        .flatMap(_.asElement)
        .headOption
        .map(_.getText)
      yield
        val bodyRaw: Xml.Nodes = element
          .getChildren
          .dropUntil(_.asElement.isDefined) // TODO why?

        val body: Xml.Nodes = bodyRaw.head.asText match
          case Some(text) if text.startsWith(".") => Xml.text(text.drop(1)) +: bodyRaw.tail
          case _ => bodyRaw

        Footnotes.bodyStub(correlationId, body)




