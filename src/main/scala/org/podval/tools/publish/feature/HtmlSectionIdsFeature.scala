package org.podval.tools.publish.feature

import org.podval.tools.publish.markup.HtmlLikeMarkup
import org.podval.tools.publish.page.PageContent
import org.podval.tools.publish.processor.{Converter, Feature}
import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.Xml

final class HtmlSectionIdsFeature extends Feature(
  converter = Some(HtmlSectionIdsFeature.HtmlSectionIdsConverter())
)

object HtmlSectionIdsFeature:
  private final class HtmlSectionIdsConverter extends Converter:
    // Note: for Markdown, this can be achieved by setting `HtmlRenderer.GENERATE_HEADER_ID`,
    // but I do it manually and uniformly for HTML, TEI etc.
    override def convert(
      element: Xml.Element,
      content: PageContent,
      ids: IdGenerator,
      footnoteCorrelationIds: IdGenerator
    ): Xml.Element =
      if element.getId.isDefined || HtmlLikeMarkup.headerLevel(element).isEmpty
      then element
      else element.setId(element.getTextOpt.fold(ids.generate())(Xml.toId))
