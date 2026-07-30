package org.podval.tools.publish.html

import org.podval.tools.publish.markup.Converter
import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.Xml

final class HtmlSectionIdsConverter(ids: IdGenerator) extends Converter:
  override def convert(element: Xml.Element): Option[Xml.Element] =
    Option.when(element.getId.isEmpty && HtmlSection.headerLevel(element).isDefined)(
      element.setId(element.getTextOpt.fold(ids.general())(Xml.toId))
    )
