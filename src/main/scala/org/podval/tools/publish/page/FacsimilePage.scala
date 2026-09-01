package org.podval.tools.publish.page

import org.podval.tools.publish.markup.Facsimile
import org.podval.tools.publish.util.Icon
import org.podval.xml.{Html, XmlUtil}
import scala.annotation.tailrec

final class FacsimilePage(
  val document: FullMarkupPage
) extends SyntheticMarkupPage(
  document.site,
  Facsimile.path(document)
):
  // Live under /P/; do not synthesize a DirectoryPage at /P/index.html.
  override lazy val parent: Option[DirectoryPage] = document.parent

  override def titleFromPath: String = document.titleFromPath

  override def titleDefault: String = s"${document.title} facsimile"

  override protected def iconDefault: Icon = Icon.images

  override def pageHeader: Option[Html.Element] = Some(PageHeader.of(document))

  override protected def formatSourcePage: Option[FullMarkupPage] = Some(document)

  override protected def formatIsFacsimile: Boolean = true

  override protected def isFacsimileViewer: Boolean = true

  override def prev: Option[Page] = nextFacsimile(document.prev, _.prev)

  override def next: Option[Page] = nextFacsimile(document.next, _.next)

  override protected def syntheticContent: Html.Element =
    XmlUtil.xml2html(Facsimile.scroller(this))

  @tailrec
  private def nextFacsimile(page: Option[Page], step: MarkupPage => Option[Page]): Option[Page] =
    page match
      case None => None
      case Some(p) =>
        site.pages.facsimilePage(p) match
          case found @ Some(_) => found
          case None => p match
            case m: MarkupPage => nextFacsimile(step(m), step)
            case _ => None
