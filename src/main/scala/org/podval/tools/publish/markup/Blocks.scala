package org.podval.tools.publish.markup

import org.podval.xml.{Xml, XmlDialect}
import org.podval.tools.publish.site.{PageError, PageErrorReporter}

final class Blocks private(blocks: Seq[Block]):
  def resolve(id: String): Option[Link.ToBlock] = blocks.find(_.id == id).map(Link.ToBlock(_))

object Blocks:
  def apply(
    xml: Xml.Element,
    xmlDialect: XmlDialect,
    errorReporter: PageErrorReporter
  ): Blocks =
    val result: Seq[Block] = xmlDialect.gather(xml, element =>
      if !element.has(Block.BlockClass) then None else element
        .getId
        .map(Block(_))
        .orElse:
          errorReporter.error(PageError.NoId, s"Defect: No id on block $element")
          None
    )
    new Blocks(result)
