package org.podval.tools.publish.feature

import org.podval.tools.publish.markup.HtmlLikeMarkup
import org.podval.xml.{Xml, XmlAttribute}

object HtmlSectionIdsFeature extends Feature:

  // Note: for Markdown, this can be achieved by setting `HtmlRenderer.GENERATE_HEADER_ID`,
  // but I do it manually and uniformly for HTML, TEI etc.
  override def process(
    element: Xml.Element,
    context: Feature.ProcessContext
  ): Xml.Element =
    if element.getId.isDefined || HtmlLikeMarkup.headerLevel(element).isEmpty
    then element
    else element.setId(element.getTextOpt.fold(context.generateId())(XmlAttribute.Id.toId))
