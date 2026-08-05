package org.podval.tools.publish.page

import org.podval.xml.Html

final class SectionChunkPage(
  markupPage: OriginalMarkupPage,
  sectionId: String,
  isTerminal: Boolean
) extends ChunkPage(
  markupPage,
  sectionId
):
  override def isDirectory: Boolean = false

  override def markupContent: Option[Html.Element] = markupPage.markupContent(
    sectionId = Some(sectionId),
    isTerminal = isTerminal
  )

  override def up: Option[Page] = None
  override def prev: Option[Page] = None
  override def next: Option[Page] = None
