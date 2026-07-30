package org.podval.tools.publish.asciidoc

import org.podval.tools.publish.markup.{Converter, Footnotes}
import org.podval.xml.Xml

// There is no way to take over Asciidoctor's footnote processing
// nor even configure the class names it emits (unlike for FlexMark).
// Here I post-process its output to the form Markup understands.
//
// Asciidoctor footnote body:
//   <div class="footnote" id="_footnotedef_N">
//     <a href="#_footnoteref_N">N</a>. Footnote Body
//   </div>
final class AsciiDocFootnoteBodiesConverter extends Converter:
  override def convert(element: Xml.Element): Option[Xml.Element] =
    if element.getName != "div" || !element.has(Footnotes.BodyClass) then None else
      val correlationId: Option[String] = element
        .getChildren
        .flatMap(_.asElement)
        .headOption
        .map(_.getText)

      val bodyRaw: Xml.Nodes = element
        .getChildren
        .dropUntil(_.asElement.isDefined)

      val body: Xml.Nodes = bodyRaw.head.asText match
        case Some(text) if text.startsWith(".") => Xml.text(text.drop(1)) +: bodyRaw.tail
        case _ => bodyRaw

      for
        correlationId <- correlationId
      yield
        // TODO drop the leading dot!
        Footnotes.bodyStub(correlationId, body)
