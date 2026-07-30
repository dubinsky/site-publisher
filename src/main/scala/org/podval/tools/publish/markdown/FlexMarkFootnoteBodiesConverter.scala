package org.podval.tools.publish.markdown

import org.podval.tools.publish.markup.{Converter, Footnotes}
import org.podval.xml.Xml

// Note: without FootnotesExtension, FlexMark treats footnotes as links,
// and by the time we get to `convertFootnotes()` footnotes are gone,
// so to process footnotes in Markdown markup correctly, I have to enable FootnotesExtension.
// Here I post-process its output to the form Markup understands.
//
// FootnotesExtension footnote body:
//   <li id="fn-$correlationId">
//     ...
//     <p>...</p>
//     ...
//     <a class="Footnotes.LinkBody.name" href="fnref-$correlationId">arrow back symbol</a>
//     ...
//   </li>
final class FlexMarkFootnoteBodiesConverter extends Converter:
  override def convert(element: Xml.Element): Option[Xml.Element] =
    if element.getName != "li" then None else
      val correlationId: Option[String] = element.getId.flatMap: id =>
        Option.when(id.startsWith("fn-"))(id.substring("fn-".length))

      val body: Option[Xml.Nodes] = Xml
        .getChildren(element)
        .flatMap(_.asElement)
        .find(_.has(Footnotes.BodyClass))
        .map(backLink => element.getChildren.takeWhile(_ ne backLink))

      for
        correlationId <- correlationId
        body <- body
      yield
        // TODO find the <p> within the body and use its children as body...
        Footnotes.bodyStub(correlationId, body)
