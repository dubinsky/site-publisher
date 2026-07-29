package org.podval.tools.publish.page

import org.podval.xml.Html

final class TocChunkPage(markupPage: OriginalMarkupPage) extends ChunkPage(
  markupPage,
  markupPage.path.fileName // TODO DirectoryPage.fileName!
):
  override def isDirectory: Boolean = false

  override def markupContent: Option[Html.Element] = markupPage.content.map(_.toHtml(
    sectionId = None,
    isTerminal = false
  ))
  
  override def up: Option[Page] = None
  override def prev: Option[Page] = None
  override def next: Option[Page] = None
