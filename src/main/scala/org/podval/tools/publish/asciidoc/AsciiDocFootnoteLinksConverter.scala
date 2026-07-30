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
//     <a id="_footnoteref_$correlationId" class="footnote" href="#_footnotedef_$correlationId">
//       $correlationId
//     </a>
//   ]
// </sup>
final class AsciiDocFootnoteLinksConverter extends Converter:
  override def convert(element: Xml.Element): Option[Xml.Element] =
    if element.getName != "sup" then None else element
      .getChildren
      .flatMap(_.asElement)
      .find(_.has(Footnotes.BodyClass))
      .map(_.getText)
      .map(Footnotes.linkStub)
