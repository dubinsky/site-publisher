package org.podval.tools.publish.features

import org.podval.xml.{Xml, XmlAttribute}

// Note: without FootnotesExtension, FlexMark treats footnotes as links,
// and by the time we get to `convertFootnotes()` footnotes are gone,
// so to process footnotes in Markdown markup correctly, I have to enable FootnotesExtension.
// Here I post-process its output to the form Markup understands.
object FlexMarkFootnotesFeature extends Feature:
  override def process(
    element: Xml.Element,
    context: Feature.ProcessContext
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
        .find(_.has(Footnotes.LinkClass))
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
          val correlationId: Option[String] = element.get(XmlAttribute.Id).flatMap: id =>
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

      .getOrElse(element)

