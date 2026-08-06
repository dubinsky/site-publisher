package org.podval.tools.publish.markup

import org.podval.xml.{Xml, XmlDialect}
import org.podval.tools.publish.site.{PageError, PageErrorReporter}

final class WikiBlocks private(blocks: Seq[WikiBlock]):
  def resolve(id: String): Option[Link.ToBlock] = blocks.find(_.id == id).map(Link.ToBlock(_))

object WikiBlocks:
  def apply(
    xml: Xml.Element,
    xmlDialect: XmlDialect,
    errorReporter: PageErrorReporter
  ): WikiBlocks = new WikiBlocks(
    xmlDialect.gather(xml, element =>
      if !WikiBlock.is(element) then None else element
        .getId
        .map(WikiBlock(_))
        .orElse:
          errorReporter.error(PageError.NoId, s"Defect: No id on block $element")
          None
    )
  )
