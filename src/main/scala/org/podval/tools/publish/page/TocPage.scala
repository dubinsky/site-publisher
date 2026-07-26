package org.podval.tools.publish.page

import org.podval.xml.Html

// TODO SectionTocPage, SectionPage
final class TocPage(markupPage: OriginalMarkupPage) extends DerivedMarkupPage(
  markupPage.site,
  markupPage.path.add("toc" /*DirectoryPage.fileName*/).html // TODO idex.html!
):
  override def isDirectory: Boolean = false

  override def up: Option[Page] = None
  override def prev: Option[Page] = None
  override def next: Option[Page] = None

  override def markupContent: Option[Html.Element] = None // TODO
  override def pageHeader: Option[Html.Element] = None // TODO
