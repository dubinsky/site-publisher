package org.podval.tools.publish.page

import org.podval.tools.publish.site.{Path, Site}
import org.podval.xml.Html

abstract class OriginalMarkupPage(site: Site, path: Path) extends MarkupPage(site, path):
  final override def prev: Option[Page] = parent.flatMap(_.prev(this))
  final override def next: Option[Page] = parent.flatMap(_.next(this))

  private var sourceVar: Option[PageSource] = None
  final override def source: Option[PageSource] = sourceVar
  final def setSource(source: PageSource): Unit = this.sourceVar = Some(source)

  final override def markupContent: Option[Html.Element] = content.map(_.toHtml(
    sectionId = None,
    isTerminal = true
  ))
  
  final override def pageHeader: Option[Html.Element] = content.map(_.source.markup.pageHeader(this))
