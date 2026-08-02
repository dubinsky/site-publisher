package org.podval.tools.publish.markdown

import org.podval.tools.publish.markup.Footnotes
import org.podval.xml.Xml

// Note: without FootnotesExtension, FlexMark treats footnotes as links,
// and by the time we get to `convertFootnotes()` footnotes are gone,
// so to process footnotes in Markdown markup correctly, I have to enable FootnotesExtension.
// Here I post-process its output to the form Markup understands.
object FlexMarkFootnotes:
  // From:
  //   <sup id="fnref-N"><a class="footnote-ref" href="#fn-N">N</a></sup>
  // To:
  //   <a class="footnote-link" footnoteCorrelationId="N"/>
  def convertFootnoteLink(element: Xml.Element): Option[Xml.Element] =
    if element.getName != "sup" then None else
      for correlationId <- element
        .getChildren
        .flatMap(_.asElement)
        .find(_.hasClass("footnote-ref"))
        .flatMap(_.getTextOpt)
      yield
        Footnotes.linkStub(correlationId)

  // From:
  //   <li id="fn-N">
  //     ...
  //     <p>...</p>
  //     ...
  //     <a class="footnote-backref" href="fnref-N">Footnote Body</a>
  //     ...
  //   </li>
  // To:
  //   <span class="footnote" footnoteCorrelationId="N">Footnote Body</span>
  def convertFootnoteBody(element: Xml.Element): Option[Xml.Element] =
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

