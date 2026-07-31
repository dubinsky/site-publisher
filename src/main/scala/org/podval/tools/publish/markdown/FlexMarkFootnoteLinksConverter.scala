package org.podval.tools.publish.markdown

import org.podval.tools.publish.markup.{Converter, Footnotes}
import org.podval.xml.Xml

// Note: without FootnotesExtension, FlexMark treats footnotes as links,
// and by the time we get to `convertFootnotes()` footnotes are gone,
// so to process footnotes in Markdown markup correctly, I have to enable FootnotesExtension.
// Here I post-process its output to the form Markup understands.
//
// FootnotesExtension footnote link:
//   <sup id="fnref-N">
//     <a class="footnote-ref" href="#fn-N">N</a>
//   </sup>
//
// Footnote link stub:
//   <a class="footnote-link" footnoteCorrelationId="N"/>
//
final class FlexMarkFootnoteLinksConverter extends Converter:
  override def convert(element: Xml.Element): Option[Xml.Element] =
    if element.getName != "sup" then None else
      for correlationId <- element
        .getChildren
        .flatMap(_.asElement)
        .find(_.hasClass("footnote-ref"))
        .flatMap(_.getTextOpt)
      yield  
        Footnotes.linkStub(correlationId)
        
