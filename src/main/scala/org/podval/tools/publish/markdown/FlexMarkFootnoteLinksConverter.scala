package org.podval.tools.publish.markdown

import org.podval.tools.publish.markup.Footnotes
import org.podval.tools.publish.processor.ConverterSimple
import org.podval.xml.Xml

// Note: without FootnotesExtension, FlexMark treats footnotes as links,
// and by the time we get to `convertFootnotes()` footnotes are gone,
// so to process footnotes in Markdown markup correctly, I have to enable FootnotesExtension.
// Here I post-process its output to the form Markup understands.
final class FlexMarkFootnoteLinksConverter extends ConverterSimple:
  override protected def convert(element: Xml.Element): Xml.Element =
    link(element).getOrElse(element)

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
      .find(_.has(Footnotes.LinkClass))
      .map(_.getText)
      .map(Footnotes.linkStub)
