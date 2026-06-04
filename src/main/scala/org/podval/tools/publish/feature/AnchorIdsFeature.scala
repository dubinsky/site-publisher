package org.podval.tools.publish.feature

import org.podval.xml.Xml

object AnchorIdsFeature extends Feature(
  processesLinks = true
):
  override def process(
    element: Xml.Element,
    context: Feature.ProcessContext
  ): Xml.Element =
    if !element.isA || element.getId.isDefined
    then element
    else element.setId(context.generateId())
