package org.podval.tools.publish.asciidoc

import org.podval.tools.publish.markup.HtmlSections
import org.podval.tools.publish.processor.Converter
import org.podval.xml.Xml

// Distill the soup of meaningless `div`s that Asciidoctor emits;
// see, for example, https://tiffnix.com/soupault#html-de-uglifier-plugin.
final class AsciiDocDivSoupConverter extends Converter:
  // TODO also unfold 'p's in 'td's and 'li's!
  
  override protected def convert(element: Xml.Element): Option[Xml.Element] = Some:
    unfold(element, element => element.getName == "div" && (
      element.getClasses.exists(cls => AsciiDocDivSoupConverter.spuriousDivClasses.contains(cls)) ||
      isSect(element)
    ))

  private def isSect(element: Xml.Element): Boolean = element
    .getChildren
    .flatMap(_.asElement)
    .headOption
    .flatMap(HtmlSections.headerLevel)
    .exists(headerLevel => element.hasClass(s"sect${headerLevel - 1}"))

  object AsciiDocDivSoupConverter:
    val spuriousDivClasses: Set[String] = Set(
      "paragraph",
      "sectionbody",
      "ulist",
      "olist",
      "quoteblock",
      "openblock",
      "content"
    )
