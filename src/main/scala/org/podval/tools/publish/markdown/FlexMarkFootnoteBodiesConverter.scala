package org.podval.tools.publish.markdown

import org.podval.tools.publish.markup.{Converter, Footnotes}
import org.podval.xml.Xml

// Note: without FootnotesExtension, FlexMark treats footnotes as links,
// and by the time we get to `convertFootnotes()` footnotes are gone,
// so to process footnotes in Markdown markup correctly, I have to enable FootnotesExtension.
// Here I post-process its output to the form Markup understands.
//
// FootnotesExtension footnote body:
//   <li id="fn-N">
//     ...
//     <p>...</p>
//     ...
//     <a class="footnote-backref" href="fnref-N">Footnote Body</a>
//     ...
//   </li>
//
// Footnote body stub:
//    <span class="footnote" footnoteCorrelationId="N">Footnote Body</span>
//
final class FlexMarkFootnoteBodiesConverter extends Converter:
  override def convert(element: Xml.Element): Option[Xml.Element] =
    if element.getName != "li" then None else
      for
        correlationId <- element
          .getId
          .flatMap: id =>
            Option.when(id.startsWith("fn-"))(id.substring("fn-".length))
        body <- Xml
          .getChildren(element)
          .flatMap(_.asElement)
          .find(_.hasClass("footnote-backref"))
          .map(backLink => element.getChildren.takeWhile(_ ne backLink))
      yield
        // TODO find the <p> within the body and use its children as body...
        Footnotes.bodyStub(correlationId, body)
