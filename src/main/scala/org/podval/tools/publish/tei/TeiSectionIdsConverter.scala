package org.podval.tools.publish.tei

import org.podval.tools.publish.markup.Converter
import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.Xml

// Sections in TEI:
//<div type="section" n="2"> // chapter", "section", "part", "subsection", etc
//  <head>Methodology</head>
//  <p>...</p>
//</div>
final class TeiSectionIdsConverter(ids: IdGenerator) extends Converter:
  // Note: for Markdown, this can be achieved by setting `HtmlRenderer.GENERATE_HEADER_ID`,
  // but I do it manually and uniformly for HTML, TEI etc.
  override def convert(element: Xml.Element): Option[Xml.Element] =
    Option.when(element.getName == "div" && element.getId.isEmpty)(
      element.setId(sectionTitle(element).fold(ids.general())(Xml.toId))
    )

  private def sectionTitle(element: Xml.Element): Option[String] = element
    .getChildren
    .flatMap(_.asElement)
    .find(element => element.getName == "head")
    .flatMap(_.getTextOpt)
