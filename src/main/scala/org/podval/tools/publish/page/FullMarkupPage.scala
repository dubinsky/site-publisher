package org.podval.tools.publish.page

import org.podval.tools.publish.site.{Path, Site}
import org.podval.xml.Html

abstract class FullMarkupPage(site: Site, path: Path) extends MarkupPage(site, path):
  final override def prev: Option[Page] = parent.flatMap(_.prev(this))
  final override def next: Option[Page] = parent.flatMap(_.next(this))

  private var sourceVar: Option[PageSource] = None
  final def setSource(source: PageSource): Unit = this.sourceVar = Some(source)
  final override def source: Option[PageSource] = sourceVar
  final override def markupContent: Option[Html.Element] = markupContent(
    sectionId = None, 
    isTerminal = true
  )
  final override def pageHeader: Option[Html.Element] = source.map(_.markup.pageHeader(this))
  final def chunks: Seq[ChunkedMarkupPage] = content.map(_.toc.chunks(this)).getOrElse(Seq.empty)
