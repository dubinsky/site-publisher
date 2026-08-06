package org.podval.tools.publish.page

import org.podval.tools.publish.util.Icon
import org.podval.xml.Html

// TODO TOC for chunked pages needs to reference chunks
// TODO intra-page links in the chunked page are no longer intra-page!
// TODO rename PageContent ids to anchors; store the section they are in
// Note: TOC - no sectionId, isTerminal = false
final class ChunkedMarkupPage(
  markupPage: FullMarkupPage,
  sectionId: Option[String],
  isTerminal: Boolean
) extends MarkupPage(
  markupPage.site,
  markupPage.path.add(
    sectionId.getOrElse(markupPage.path.fileName) // TODO TOC: DirectoryPage.fileName!
  ).html
):
  override def markupContent: Option[Html.Element] = markupPage.markupContent(
    sectionId = sectionId,
    isTerminal = isTerminal
  )
  
  override def isDirectory: Boolean = false

  override def source: Option[PageSource] = None
  
  override def titleFromPath: String = path.fileName

  override def hasSyntheticContent: Boolean = false

  override protected def syntheticContentOpt: Option[Html.Element] = None

  override protected def iconDefault: Icon = Icon.note // TODO page/document...

  override def pageHeader: Option[Html.Element] = markupPage.pageHeader

  override def up: Option[Page] = None

  override def prev: Option[Page] = None

  override def next: Option[Page] = None

  
