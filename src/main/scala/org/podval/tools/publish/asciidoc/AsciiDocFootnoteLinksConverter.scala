package org.podval.tools.publish.asciidoc

import org.podval.tools.publish.markup.{Converter, Footnotes}
import org.podval.xml.Xml

// There is no way to take over Asciidoctor's footnote processing
// nor even configure the class names it emits (unlike for FlexMark).
// Here I post-process its output to the form Markup understands.
//
// Asciidoctor footnote link:
// <sup class="footnote">
//   [
//     <a id="_footnoteref_N" class="footnote" href="#_footnotedef_N">
//       N
//     </a>
//   ]
// </sup>
//
// Footnote link stub:
//   <a class="footnote-link" footnoteCorrelationId="N"/>
//
final class AsciiDocFootnoteLinksConverter extends Converter:
  override def convert(element: Xml.Element): Option[Xml.Element] =
    val isFootnoteLink: Boolean = element.getName == "sup" // && element.hasClass("footnote")
    if !isFootnoteLink then None else
      for correlationId <- element
        .getChildren
        .flatMap(_.asElement)
        .find(_.hasClass("footnote"))
        .flatMap(_.getTextOpt)
      yield
        Footnotes.linkStub(correlationId)
        
       
    
