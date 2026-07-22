package org.podval.tools.publish.markdown

import org.podval.tools.publish.markup.Footnotes
import org.podval.tools.publish.processor.ConverterSimple
import org.podval.xml.Xml

// Note: without FootnotesExtension, FlexMark treats footnotes as links,
// and by the time we get to `convertFootnotes()` footnotes are gone,
// so to process footnotes in Markdown markup correctly, I have to enable FootnotesExtension.
// Here I post-process its output to the form Markup understands.
// TODO split into two!
final class FlexMarkFootnotesConverter extends ConverterSimple:
  override protected def convert(element: Xml.Element): Xml.Element =
    link(element).orElse(body(element)).getOrElse(element)

  // FootnotesExtension footnote link:
  //   <sup id="fnref-$correlationId">
  //     <a class="${Footnotes.LinkClass.name}" href="#fn-$correlationId">
  //       $correlationId
  //     </a>
  //   </sup>
  private def link(element: Xml.Element): Option[Xml.Element] =
    if element.getName != "sup" then None else element
      .getChildren
      .flatMap(_.asElement)
      .find(Footnotes.isLink)
      .map(_.getText)
      .map(Footnotes.linkStub)

  // FootnotesExtension footnote body:
  //   <li id="fn-$correlationId">
  //     ...
  //     <p>...</p>
  //     ...
  //     <a class="Footnotes.LinkBody.name" href="fnref-$correlationId">arrow back symbol</a>
  //     ...
  //   </li>
  private def body(element: Xml.Element): Option[Xml.Element] =
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
