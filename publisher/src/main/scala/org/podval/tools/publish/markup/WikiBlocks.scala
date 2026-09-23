package org.podval.tools.publish.markup

import org.podval.xml.Xml
import org.podval.tools.publish.site.{PageError, PageErrorReporter}

final class WikiBlocks private(blocks: Seq[WikiBlock]):
  def resolve(id: String): Option[Link.ToBlock] = blocks.find(_.id == id).map(Link.ToBlock(_))

object WikiBlocks:
  val empty: WikiBlocks = new WikiBlocks(Seq.empty)

  def apply(
    xml: Xml.Element,
    errorReporter: PageErrorReporter
  ): WikiBlocks = new WikiBlocks(
    xml.gather(element =>
      if !WikiBlock.is(element) then None else element
        .getId
        .map(WikiBlock(_))
        .orElse:
          errorReporter.error(PageError.NoId, s"Defect: No id on block $element")
          None
    )
  )
