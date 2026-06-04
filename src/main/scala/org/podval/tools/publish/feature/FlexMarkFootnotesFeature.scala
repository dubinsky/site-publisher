package org.podval.tools.publish.feature

import org.podval.tools.publish.page.PageSource
import org.podval.tools.publish.processor.{Converter, Feature}
import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.Xml

// Note: without FootnotesExtension, FlexMark treats footnotes as links,
// and by the time we get to `convertFootnotes()` footnotes are gone,
// so to process footnotes in Markdown markup correctly, I have to enable FootnotesExtension.
// Here I post-process its output to the form Markup understands.
final class FlexMarkFootnotesFeature extends Feature(
  converter = Some(FlexMarkFootnotesFeature.FlexMarkFootnotesConverter())
)

object FlexMarkFootnotesFeature:
  private final class FlexMarkFootnotesConverter extends Converter:
    override def convert(
      element: Xml.Element,
      pageSource: PageSource,
      ids: IdGenerator,
      footnoteCorrelationIds: IdGenerator
    ): Xml.Element =
      // FootnotesExtension footnote link:
      //   <sup id="fnref-$correlationId">
      //     <a class="${Footnotes.LinkClass.name}" href="#fn-$correlationId">
      //       correlationId
      //     </a>
      //   </sup>
      (
        if element.getName != "sup" then None else element
          .getChildren
          .flatMap(_.asElement)
          .find(Footnotes.isLink)
          .map(_.getText)
          .map(Footnotes.linkStub)
        )
        // FootnotesExtension footnote body:
        //   <li id="fn-$correlationId">
        //     ...
        //     <p>...</p>
        //     ...
        //     <a class="Footnotes.LinkBody.name" href="fnref-$correlationId">arrow back symbol</a>
        //     ...
        //   </li>
        .orElse:
          if element.getName != "li" then None else
            val correlationId: Option[String] = element.getId.flatMap: id =>
              Option.when(id.startsWith("fn-"))(id.substring("fn-".length))
  
            val body: Option[Xml.Nodes] = Xml
              .getChildren(element)
              .flatMap(_.asElement)
              .find(Footnotes.isBody)
              .map(backLink => element.getChildren.takeWhile(_ ne backLink))
  
            for
              correlationId <- correlationId
              body <- body
            yield
              // TODO find the <p> within the body and use its children as body...
              Footnotes.bodyStub(correlationId, body)
  
        .getOrElse(element)
  
