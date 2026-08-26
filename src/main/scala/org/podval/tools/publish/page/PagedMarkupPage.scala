package org.podval.tools.publish.page

import org.podval.tools.publish.site.Posts
import org.podval.tools.publish.util.Icon
import org.podval.xml.Html

/** One batch of the posts listing. Page 1 stays `/posts.html`. */
final class PagedMarkupPage(
  postsPage: Posts,
  val batchIndex: Int,
  name: String
) extends MarkupPage(
  postsPage.site,
  postsPage.path.add(name).html
):
  override def markupContent: Option[Html.Element] = postsPage.markupContent

  override def isDirectory: Boolean = false

  override def source: Option[PageSource] = None

  override def titleFromPath: String = path.fileName

  override def titleDefault: String = s"${postsPage.title} (page $batchIndex)"

  override def hasSyntheticContent: Boolean = true

  override protected def syntheticContentOpt: Option[Html.Element] =
    Some(postsPage.batchContent(batchIndex))

  override protected def iconDefault: Icon = postsPage.icon

  override def pageHeader: Option[Html.Element] = postsPage.pageHeader

  override def up: Option[Page] = Some(postsPage)

  override def prev: Option[Page] =
    val all: Seq[Page] = postsPage.pager
    val i: Int = all.indexWhere(_.path == path)
    Option.when(i > 0)(all(i - 1))

  override def next: Option[Page] =
    val all: Seq[Page] = postsPage.pager
    val i: Int = all.indexWhere(_.path == path)
    Option.when(i >= 0 && i < all.length - 1)(all(i + 1))

  override def pagerPrev: Option[Page] = prev
  override def pagerNext: Option[Page] = next
