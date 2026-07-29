package org.podval.tools.publish.tei

import org.podval.tools.publish.markup.Converter
import org.podval.tools.publish.page.PageSource
import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.Xml

final class TeiSectionIdsConverter extends Converter:
  // Note: for Markdown, this can be achieved by setting `HtmlRenderer.GENERATE_HEADER_ID`,
  // but I do it manually and uniformly for HTML, TEI etc.
  override protected def convert(
    element: Xml.Element,
    source: PageSource,
    ids: IdGenerator,
    footnoteCorrelationIds: IdGenerator
  ): Option[Xml.Element] =
    Option.when(element.getName == "div" && element.getId.isEmpty)(
      element.setId(sectionTitle(element).fold(ids.generate())(Xml.toId))
    )

  private def sectionTitle(element: Xml.Element): Option[String] = element
    .getChildren
    .flatMap(_.asElement)
    .find(element => element.getName == "head")
    .flatMap(_.getTextOpt)
