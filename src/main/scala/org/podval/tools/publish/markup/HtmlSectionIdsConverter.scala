package org.podval.tools.publish.markup

import org.podval.tools.publish.markup.HtmlLikeMarkup
import org.podval.tools.publish.page.PageContent
import org.podval.tools.publish.processor.ConverterWithIds
import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.Xml

final class HtmlSectionIdsConverter extends ConverterWithIds:
  // Note: for Markdown, this can be achieved by setting `HtmlRenderer.GENERATE_HEADER_ID`,
  // but I do it manually and uniformly for HTML, TEI etc.
  override def convertWithIds(
    element: Xml.Element,
    content: PageContent,
    ids: IdGenerator,
    footnoteCorrelationIds: IdGenerator
  ): Xml.Element =
    if element.getId.isDefined || HtmlLikeMarkup.headerLevel(element).isEmpty
    then element
    else element.setId(element.getTextOpt.fold(ids.generate())(Xml.toId))
