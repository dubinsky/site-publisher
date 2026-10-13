package org.podval.tools.publish.page

import org.podval.tools.publish.util.Icon

/** Published TEI beside the HTML page (`P.xml`). Generated; the git file stays the source. */
final class TeiDocumentPage(
  val markupPage: FullMarkupPage
) extends Page(
  markupPage.site,
  markupPage.path.withExtension(TeiDocumentPage.extension)
) with PageWithContent:
  override protected def iconDefault: Icon = Icon.file

  override def textContent: String = TeiStamp.render(markupPage)

object TeiDocumentPage:
  val extension: String = "xml"
