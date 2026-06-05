package org.podval.tools.publish.feature

import org.podval.tools.publish.page.PageSource
import org.podval.tools.publish.processor.{Converter, Feature}
import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.Xml

final class TeiSectionIdsFeature extends Feature(
  converter = Some(TeiSectionIdsFeature.TeiSectionIdsConverter())
)

object TeiSectionIdsFeature:
  private final class TeiSectionIdsConverter extends Converter:
    // Note: for Markdown, this can be achieved by setting `HtmlRenderer.GENERATE_HEADER_ID`,
    // but I do it manually and uniformly for HTML, TEI etc.
    override def convert(
      element: Xml.Element,
      source: PageSource,
      ids: IdGenerator,
      footnoteCorrelationIds: IdGenerator
    ): Xml.Element =
      if element.getName != "div" || element.getId.isDefined
      then element
      else element.setId(sectionTitle(element).fold(ids.generate())(Xml.toId))

    private def sectionTitle(element: Xml.Element): Option[String] = element
      .getChildren
      .flatMap(_.asElement)
      .find(element => element.getName == "head")
      .flatMap(_.getTextOpt)
