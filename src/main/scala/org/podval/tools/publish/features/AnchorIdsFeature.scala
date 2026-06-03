package org.podval.tools.publish.features

import org.podval.xml.{HtmlElement, Xml, XmlAttribute}

object AnchorIdsFeature extends Feature(
  // Note: process this last, so that everything that was to be converted to a link had:
  processPriority = 100
):
  override def process(
    element: Xml.Element,
    context: Feature.ProcessContext
  ): Xml.Element =
    if !element.isElement(HtmlElement.A) || element.get(XmlAttribute.Id).isDefined
    then element
    else element.set(XmlAttribute.Id, context.generateId())
