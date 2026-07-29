package org.podval.tools.publish.html

import org.podval.tools.publish.markup.Converter
import org.podval.tools.publish.page.PageSource
import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.Xml

final class HtmlSectionIdsConverter extends Converter:
  override protected def convert(
    element: Xml.Element,
    source: PageSource,
    ids: IdGenerator,
    footnoteCorrelationIds: IdGenerator
  ): Option[Xml.Element] =
    Option.when(element.getId.isEmpty && HtmlSection.headerLevel(element).isDefined)(
      element.setId(element.getTextOpt.fold(ids.generate())(Xml.toId))
    )
