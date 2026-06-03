package org.podval.tools.publish.features

import org.podval.xml.{Xml, XmlAttribute}

object TeiSectionIdsFeature extends Feature:

  // Note: for Markdown, this can be achieved by setting `HtmlRenderer.GENERATE_HEADER_ID`,
  // but I do it manually and uniformly for HTML, TEI etc.
  override def process(
    element: Xml.Element,
    context: Feature.ProcessContext
  ): Xml.Element =
    if element.getName != "div" || element.get(XmlAttribute.Id).isDefined
    then element
    else element.set(XmlAttribute.Id, sectionTitle(element).fold(context.generateId())(XmlAttribute.Id.toId))

  private def sectionTitle(element: Xml.Element): Option[String] = element
    .getChildren
    .flatMap(_.asElement)
    .find(element => element.getName == "head")
    .flatMap(_.getTextOpt)  
