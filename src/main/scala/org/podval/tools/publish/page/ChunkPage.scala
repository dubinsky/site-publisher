package org.podval.tools.publish.page

import org.podval.tools.publish.util.Icon
import org.podval.xml.Html

// TODO TOC for chunked pages needs to reference chunks
// TODO intra-page links in the chunked page are no longer intra-page!
// TODO transform HTML sections to TEI-style nested ones!
// TODO rename PageContent ids to anchors; store the section they are in

abstract class ChunkPage(
  markupPage: OriginalMarkupPage,
  name: String
) extends MarkupPage(
  markupPage.site,
  markupPage.path.add(name).html
):
  final override def source: Option[PageSource] = None
  
  final override def titleFromPath: String = path.fileName

  final override def hasSyntheticContent: Boolean = false

  final override protected def syntheticContentOpt: Option[Html.Element] = None

  final override protected def iconDefault: Icon = Icon.note // TODO page/document...

  final override def pageHeader: Option[Html.Element] = markupPage.pageHeader
  
